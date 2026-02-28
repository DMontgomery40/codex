use crate::agent::AgentStatus;
use crate::agent::exceeds_thread_spawn_depth_limit;
use crate::agent::status::is_final;
use crate::codex::Session;
use crate::codex::TurnContext;
use crate::config::Config;
use crate::error::CodexErr;
use crate::features::Feature;
use crate::function_tool::FunctionCallError;
use crate::state::TeamMember;
use crate::tools::context::ToolInvocation;
use crate::tools::context::ToolOutput;
use crate::tools::context::ToolPayload;
use crate::tools::handlers::parse_arguments;
use crate::tools::registry::ToolHandler;
use crate::tools::registry::ToolKind;
use async_trait::async_trait;
use codex_protocol::ThreadId;
use codex_protocol::models::BaseInstructions;
use codex_protocol::models::FunctionCallOutputBody;
use codex_protocol::protocol::CollabAgentInteractionBeginEvent;
use codex_protocol::protocol::CollabAgentInteractionEndEvent;
use codex_protocol::protocol::CollabAgentRef;
use codex_protocol::protocol::CollabAgentSpawnBeginEvent;
use codex_protocol::protocol::CollabAgentSpawnEndEvent;
use codex_protocol::protocol::CollabAgentStatusEntry;
use codex_protocol::protocol::CollabCloseBeginEvent;
use codex_protocol::protocol::CollabCloseEndEvent;
use codex_protocol::protocol::CollabResumeBeginEvent;
use codex_protocol::protocol::CollabResumeEndEvent;
use codex_protocol::protocol::CollabWaitingBeginEvent;
use codex_protocol::protocol::CollabWaitingEndEvent;
use codex_protocol::protocol::SessionSource;
use codex_protocol::protocol::SubAgentSource;
use codex_protocol::user_input::UserInput;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

pub struct MultiAgentHandler;

/// Minimum wait timeout to prevent tight polling loops from burning CPU.
pub(crate) const MIN_WAIT_TIMEOUT_MS: i64 = 10_000;
pub(crate) const DEFAULT_WAIT_TIMEOUT_MS: i64 = 30_000;
pub(crate) const MAX_WAIT_TIMEOUT_MS: i64 = 3600 * 1000;

#[derive(Debug, Deserialize, Serialize)]
struct CloseAgentArgs {
    id: String,
}

#[async_trait]
impl ToolHandler for MultiAgentHandler {
    fn kind(&self) -> ToolKind {
        ToolKind::Function
    }

    fn matches_kind(&self, payload: &ToolPayload) -> bool {
        matches!(payload, ToolPayload::Function { .. })
    }

    async fn handle(&self, invocation: ToolInvocation) -> Result<ToolOutput, FunctionCallError> {
        let ToolInvocation {
            session,
            turn,
            tool_name,
            payload,
            call_id,
            ..
        } = invocation;

        let arguments = match payload {
            ToolPayload::Function { arguments } => arguments,
            _ => {
                return Err(FunctionCallError::RespondToModel(
                    "collab handler received unsupported payload".to_string(),
                ));
            }
        };

        match tool_name.as_str() {
            "spawn_agent" => spawn::handle(session, turn, call_id, arguments).await,
            "spawn_team" => team::spawn_team(session, turn, call_id, arguments).await,
            "list_teams" => team::list_teams(session, call_id, arguments).await,
            "get_team" => team::get_team(session, call_id, arguments).await,
            "send_input" => send_input::handle(session, turn, call_id, arguments).await,
            "team_member_status" => team::team_member_status(session, call_id, arguments).await,
            "team_message" => team::team_message(session, turn, call_id, arguments).await,
            "team_broadcast" => team::team_broadcast(session, turn, call_id, arguments).await,
            "resume_agent" => resume_agent::handle(session, turn, call_id, arguments).await,
            "wait" => wait::handle(session, turn, call_id, arguments).await,
            "wait_team" => team::wait_team(session, turn, call_id, arguments).await,
            "close_agent" => close_agent::handle(session, turn, call_id, arguments).await,
            "close_team" => team::close_team(session, turn, call_id, arguments).await,
            "team_cleanup" => team::team_cleanup(session, turn, call_id, arguments).await,
            other => Err(FunctionCallError::RespondToModel(format!(
                "unsupported collab tool {other}"
            ))),
        }
    }
}

mod spawn {
    use super::*;
    use crate::agent::control::SpawnAgentOptions;
    use crate::agent::role::DEFAULT_ROLE_NAME;
    use crate::agent::role::apply_role_to_config;

    use crate::agent::exceeds_thread_spawn_depth_limit;
    use crate::agent::next_thread_spawn_depth;
    use std::sync::Arc;

    #[derive(Debug, Deserialize)]
    struct SpawnAgentArgs {
        message: Option<String>,
        items: Option<Vec<UserInput>>,
        agent_type: Option<String>,
        #[serde(default)]
        fork_context: bool,
    }

    #[derive(Debug, Serialize)]
    struct SpawnAgentResult {
        agent_id: String,
        nickname: Option<String>,
    }

    pub async fn handle(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: SpawnAgentArgs = parse_arguments(&arguments)?;
        let role_name = args
            .agent_type
            .as_deref()
            .map(str::trim)
            .filter(|role| !role.is_empty());
        let input_items = parse_collab_input(args.message, args.items)?;
        let prompt = input_preview(&input_items);
        let session_source = turn.session_source.clone();
        let child_depth = next_thread_spawn_depth(&session_source);
        let max_depth = turn.config.agent_max_depth;
        if exceeds_thread_spawn_depth_limit(child_depth, max_depth) {
            return Err(FunctionCallError::RespondToModel(
                "Agent depth limit reached. Solve the task yourself.".to_string(),
            ));
        }
        session
            .send_event(
                &turn,
                CollabAgentSpawnBeginEvent {
                    call_id: call_id.clone(),
                    sender_thread_id: session.conversation_id,
                    prompt: prompt.clone(),
                }
                .into(),
            )
            .await;
        let mut config =
            build_agent_spawn_config(&session.get_base_instructions().await, turn.as_ref())?;
        apply_role_to_config(&mut config, role_name)
            .await
            .map_err(FunctionCallError::RespondToModel)?;
        apply_spawn_agent_runtime_overrides(&mut config, turn.as_ref())?;
        apply_spawn_agent_overrides(&mut config, child_depth);

        let result = session
            .services
            .agent_control
            .spawn_agent_with_options(
                config,
                input_items,
                Some(thread_spawn_source(
                    session.conversation_id,
                    child_depth,
                    role_name,
                )),
                SpawnAgentOptions {
                    fork_parent_spawn_call_id: args.fork_context.then(|| call_id.clone()),
                },
            )
            .await
            .map_err(collab_spawn_error);
        let (new_thread_id, status) = match &result {
            Ok(thread_id) => (
                Some(*thread_id),
                session.services.agent_control.get_status(*thread_id).await,
            ),
            Err(_) => (None, AgentStatus::NotFound),
        };
        let (new_agent_nickname, new_agent_role) = match new_thread_id {
            Some(thread_id) => session
                .services
                .agent_control
                .get_agent_nickname_and_role(thread_id)
                .await
                .unwrap_or((None, None)),
            None => (None, None),
        };
        let nickname = new_agent_nickname.clone();
        session
            .send_event(
                &turn,
                CollabAgentSpawnEndEvent {
                    call_id,
                    sender_thread_id: session.conversation_id,
                    new_thread_id,
                    new_agent_nickname,
                    new_agent_role,
                    prompt,
                    status,
                }
                .into(),
            )
            .await;
        let new_thread_id = result?;
        let role_tag = role_name.unwrap_or(DEFAULT_ROLE_NAME);
        turn.otel_manager
            .counter("codex.multi_agent.spawn", 1, &[("role", role_tag)]);

        let content = serde_json::to_string(&SpawnAgentResult {
            agent_id: new_thread_id.to_string(),
            nickname,
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize spawn_agent result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }
}

mod team {
    use super::*;
    use crate::state::TeamState;
    use futures::StreamExt;
    use futures::stream::FuturesUnordered;
    use serde_json::json;
    use std::collections::BTreeMap;
    use std::collections::HashSet;
    use std::sync::Arc;

    #[derive(Debug, Deserialize)]
    struct SpawnTeamArgs {
        team_name: Option<String>,
        lead_name: Option<String>,
        #[serde(default)]
        auto_cleanup: bool,
        agents: Vec<SpawnTeamMemberArgs>,
    }

    #[derive(Debug, Deserialize)]
    struct SpawnTeamMemberArgs {
        name: String,
        message: Option<String>,
        items: Option<Vec<UserInput>>,
        agent_type: Option<String>,
        #[serde(default)]
        fork_context: bool,
    }

    #[derive(Debug, Serialize)]
    struct SpawnTeamResult {
        team_id: String,
        team_name: String,
        lead_name: String,
        auto_cleanup: bool,
        members: Vec<TeamMemberResult>,
    }

    #[derive(Debug, Serialize)]
    struct TeamMemberResult {
        name: String,
        agent_id: String,
        nickname: Option<String>,
        role: Option<String>,
    }

    #[derive(Debug, Deserialize)]
    struct TeamMessageArgs {
        team_id: String,
        to: String,
        message: Option<String>,
        items: Option<Vec<UserInput>>,
        #[serde(default)]
        interrupt: bool,
        from: Option<String>,
    }

    #[derive(Debug, Serialize)]
    struct TeamMessageResult {
        team_id: String,
        from: String,
        to: String,
        submission_id: String,
    }

    #[derive(Debug, Deserialize)]
    struct TeamBroadcastArgs {
        team_id: String,
        message: Option<String>,
        items: Option<Vec<UserInput>>,
        #[serde(default)]
        interrupt: bool,
        #[serde(default)]
        include_sender: bool,
        from: Option<String>,
    }

    #[derive(Debug, Serialize)]
    struct TeamBroadcastResult {
        team_id: String,
        from: String,
        delivered: BTreeMap<String, String>,
        failed: BTreeMap<String, String>,
    }

    #[derive(Debug, Deserialize)]
    struct WaitTeamArgs {
        team_id: String,
        timeout_ms: Option<i64>,
        include_lead: Option<bool>,
        members: Option<Vec<String>>,
        #[serde(default)]
        wait_for_all: bool,
        #[serde(default)]
        auto_cleanup: bool,
    }

    #[derive(Debug, Serialize)]
    struct WaitTeamResult {
        team_id: String,
        timed_out: bool,
        status: BTreeMap<String, AgentStatus>,
    }

    #[derive(Debug, Deserialize, Serialize)]
    struct CloseTeamArgs {
        team_id: String,
        include_lead: Option<bool>,
        #[serde(default)]
        remove: bool,
    }

    #[derive(Debug, Deserialize)]
    struct TeamCleanupArgs {
        team_id: String,
        include_lead: Option<bool>,
    }

    #[derive(Debug, Deserialize)]
    struct GetTeamArgs {
        team_id: String,
    }

    #[derive(Debug, Deserialize)]
    struct TeamMemberStatusArgs {
        team_id: String,
        include_lead: Option<bool>,
        members: Option<Vec<String>>,
    }

    #[derive(Debug, Deserialize)]
    struct ListTeamsArgs {}

    #[derive(Debug, Serialize)]
    struct TeamSummary {
        team_id: String,
        team_name: String,
        lead_name: String,
        member_count: usize,
        auto_cleanup: bool,
    }

    #[derive(Debug, Serialize)]
    struct ListTeamsResult {
        teams: Vec<TeamSummary>,
    }

    #[derive(Debug, Serialize)]
    struct GetTeamResult {
        team_id: String,
        team_name: String,
        lead_name: String,
        auto_cleanup: bool,
        members: Vec<TeamMemberResult>,
    }

    #[derive(Debug, Serialize)]
    struct TeamMemberStatusResult {
        team_id: String,
        status: BTreeMap<String, AgentStatus>,
    }

    #[derive(Debug, Serialize)]
    struct CloseTeamResult {
        team_id: String,
        removed: bool,
        status: BTreeMap<String, AgentStatus>,
    }

    #[derive(Debug, Serialize)]
    struct SpawnAgentProxyArgs {
        #[serde(skip_serializing_if = "Option::is_none")]
        message: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        items: Option<Vec<UserInput>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        agent_type: Option<String>,
        #[serde(default)]
        fork_context: bool,
    }

    #[derive(Debug, Deserialize)]
    struct SpawnAgentProxyResult {
        agent_id: String,
        nickname: Option<String>,
    }

    #[derive(Debug, Serialize)]
    struct SendInputProxyArgs {
        id: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        message: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        items: Option<Vec<UserInput>>,
        #[serde(default)]
        interrupt: bool,
    }

    #[derive(Debug, Deserialize)]
    struct SendInputProxyResult {
        submission_id: String,
    }

    pub async fn spawn_team(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: SpawnTeamArgs = parse_arguments(&arguments)?;
        if args.agents.is_empty() {
            return Err(FunctionCallError::RespondToModel(
                "agents must be non-empty".to_string(),
            ));
        }

        let lead_name = args
            .lead_name
            .as_deref()
            .map(str::trim)
            .filter(|name| !name.is_empty())
            .unwrap_or("orchestrator")
            .to_string();

        let mut seen_names = HashSet::new();
        seen_names.insert(lead_name.to_ascii_lowercase());
        for member in &args.agents {
            let member_name = member.name.trim();
            if member_name.is_empty() {
                return Err(FunctionCallError::RespondToModel(
                    "team member name cannot be empty".to_string(),
                ));
            }
            let key = member_name.to_ascii_lowercase();
            if !seen_names.insert(key) {
                return Err(FunctionCallError::RespondToModel(format!(
                    "duplicate team member name: {member_name}"
                )));
            }
        }

        let mut spawned_thread_ids = Vec::new();
        let mut spawned_members = Vec::with_capacity(args.agents.len());

        for (index, member) in args.agents.iter().enumerate() {
            let proxy_args = SpawnAgentProxyArgs {
                message: member.message.clone(),
                items: member.items.clone(),
                agent_type: member.agent_type.clone(),
                fork_context: member.fork_context,
            };
            let serialized = serde_json::to_string(&proxy_args).map_err(|err| {
                FunctionCallError::Fatal(format!("failed to serialize spawn_team args: {err}"))
            })?;
            let nested_call_id = format!("{call_id}:{index}");
            let spawn_output =
                spawn::handle(session.clone(), turn.clone(), nested_call_id, serialized).await;
            let spawn_output = match spawn_output {
                Ok(output) => output,
                Err(err) => {
                    shutdown_spawned_agents(session.as_ref(), &spawned_thread_ids).await;
                    return Err(err);
                }
            };
            let body = tool_output_text(spawn_output)?;
            let parsed: SpawnAgentProxyResult = serde_json::from_str(&body).map_err(|err| {
                FunctionCallError::Fatal(format!(
                    "failed to parse spawn_agent result for spawn_team: {err}"
                ))
            })?;
            let thread_id = agent_id(&parsed.agent_id)?;
            spawned_thread_ids.push(thread_id);
            let (agent_nickname, agent_role) = session
                .services
                .agent_control
                .get_agent_nickname_and_role(thread_id)
                .await
                .unwrap_or((parsed.nickname.clone(), None));
            spawned_members.push(TeamMember {
                name: member.name.trim().to_string(),
                thread_id,
                nickname: parsed.nickname.or(agent_nickname),
                role: agent_role.or_else(|| Some("runtime-agent".to_string())),
            });
        }

        let created_team = {
            let team_registry = session.services.agent_control.team_registry();
            let mut registry = team_registry.lock().await;
            registry.create_team(
                args.team_name.as_deref(),
                TeamMember {
                    name: lead_name.clone(),
                    thread_id: session.conversation_id,
                    nickname: None,
                    role: Some("team-lead".to_string()),
                },
                spawned_members.clone(),
                args.auto_cleanup,
            )
        };

        let created_team = match created_team {
            Ok(team) => team,
            Err(err) => {
                shutdown_spawned_agents(session.as_ref(), &spawned_thread_ids).await;
                return Err(FunctionCallError::RespondToModel(err));
            }
        };

        let mut members = created_team
            .members()
            .map(|member| TeamMemberResult {
                name: member.name.clone(),
                agent_id: member.thread_id.to_string(),
                nickname: member.nickname.clone(),
                role: member.role.clone(),
            })
            .collect::<Vec<_>>();
        members.sort_by(|left, right| left.name.cmp(&right.name));

        let content = serde_json::to_string(&SpawnTeamResult {
            team_id: created_team.team_id,
            team_name: created_team.team_name,
            lead_name: created_team.lead_name,
            auto_cleanup: created_team.auto_cleanup,
            members,
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize spawn_team result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }

    pub async fn list_teams(
        session: Arc<Session>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let _args: ListTeamsArgs = parse_arguments(&arguments)?;
        let team_registry = session.services.agent_control.team_registry();
        let registry = team_registry.lock().await;
        let mut teams = registry
            .teams()
            .into_iter()
            .map(|team| {
                let member_count = team.member_count();
                TeamSummary {
                    team_id: team.team_id,
                    team_name: team.team_name,
                    lead_name: team.lead_name,
                    member_count,
                    auto_cleanup: team.auto_cleanup,
                }
            })
            .collect::<Vec<_>>();
        teams.sort_by(|left, right| left.team_name.cmp(&right.team_name));

        let _ = call_id;
        let content = serde_json::to_string(&ListTeamsResult { teams }).map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize list_teams result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }

    pub async fn get_team(
        session: Arc<Session>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: GetTeamArgs = parse_arguments(&arguments)?;
        let team = team_snapshot(session.as_ref(), &args.team_id).await?;
        let mut members = team
            .members()
            .map(|member| TeamMemberResult {
                name: member.name.clone(),
                agent_id: member.thread_id.to_string(),
                nickname: member.nickname.clone(),
                role: member.role.clone(),
            })
            .collect::<Vec<_>>();
        members.sort_by(|left, right| left.name.cmp(&right.name));

        let _ = call_id;
        let content = serde_json::to_string(&GetTeamResult {
            team_id: team.team_id,
            team_name: team.team_name,
            lead_name: team.lead_name,
            auto_cleanup: team.auto_cleanup,
            members,
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize get_team result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }

    pub async fn team_member_status(
        session: Arc<Session>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: TeamMemberStatusArgs = parse_arguments(&arguments)?;
        let team = team_snapshot(session.as_ref(), &args.team_id).await?;
        let include_lead = args.include_lead.unwrap_or(false);
        let member_filter = parse_member_filter(args.members)?;
        let mut members = team
            .members()
            .filter(|member| include_lead || member.thread_id != team.lead_thread_id)
            .filter(|member| {
                member_filter
                    .as_ref()
                    .is_none_or(|filter| filter.contains(&member.name.to_ascii_lowercase()))
            })
            .cloned()
            .collect::<Vec<_>>();
        members.sort_by(|left, right| left.name.cmp(&right.name));
        if members.is_empty() {
            return Err(FunctionCallError::RespondToModel(
                "team_member_status has no members to query".to_string(),
            ));
        }

        let mut status = BTreeMap::new();
        for member in members {
            let member_status = session
                .services
                .agent_control
                .get_status(member.thread_id)
                .await;
            status.insert(member.name, member_status);
        }

        let _ = call_id;
        let content = serde_json::to_string(&TeamMemberStatusResult {
            team_id: args.team_id,
            status,
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!(
                "failed to serialize team_member_status result: {err}"
            ))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }

    pub async fn team_message(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: TeamMessageArgs = parse_arguments(&arguments)?;
        let team = team_snapshot(session.as_ref(), &args.team_id).await?;
        let sender = resolve_sender(
            &team,
            session.conversation_id,
            args.from.as_deref(),
            "team_message",
        )?;
        let recipient = team.member(&args.to).cloned().ok_or_else(|| {
            FunctionCallError::RespondToModel(format!(
                "team_message target not found in team {}: {}",
                args.team_id, args.to
            ))
        })?;

        let send_output = send_input::handle(
            session,
            turn,
            call_id,
            serde_json::to_string(&SendInputProxyArgs {
                id: recipient.thread_id.to_string(),
                message: args.message,
                items: args.items,
                interrupt: args.interrupt,
            })
            .map_err(|err| {
                FunctionCallError::Fatal(format!("failed to serialize team_message args: {err}"))
            })?,
        )
        .await?;
        let body = tool_output_text(send_output)?;
        let parsed: SendInputProxyResult = serde_json::from_str(&body).map_err(|err| {
            FunctionCallError::Fatal(format!("failed to parse team_message result: {err}"))
        })?;

        let content = serde_json::to_string(&TeamMessageResult {
            team_id: args.team_id,
            from: sender.name,
            to: recipient.name,
            submission_id: parsed.submission_id,
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize team_message result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }

    pub async fn team_broadcast(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: TeamBroadcastArgs = parse_arguments(&arguments)?;
        let team = team_snapshot(session.as_ref(), &args.team_id).await?;
        let sender = resolve_sender(
            &team,
            session.conversation_id,
            args.from.as_deref(),
            "team_broadcast",
        )?;

        let mut recipients = team.members().cloned().collect::<Vec<_>>();
        recipients.sort_by(|left, right| left.name.cmp(&right.name));
        if !args.include_sender {
            recipients.retain(|member| member.thread_id != sender.thread_id);
        }
        if recipients.is_empty() {
            return Err(FunctionCallError::RespondToModel(
                "team_broadcast has no recipients".to_string(),
            ));
        }

        let mut delivered = BTreeMap::new();
        let mut failed = BTreeMap::new();

        for recipient in recipients {
            let nested_call_id = format!("{call_id}:{}", recipient.name);
            let send_output = send_input::handle(
                session.clone(),
                turn.clone(),
                nested_call_id,
                serde_json::to_string(&SendInputProxyArgs {
                    id: recipient.thread_id.to_string(),
                    message: args.message.clone(),
                    items: args.items.clone(),
                    interrupt: args.interrupt,
                })
                .map_err(|err| {
                    FunctionCallError::Fatal(format!(
                        "failed to serialize team_broadcast args: {err}"
                    ))
                })?,
            )
            .await;
            match send_output {
                Ok(output) => {
                    let body = tool_output_text(output)?;
                    let parsed: SendInputProxyResult =
                        serde_json::from_str(&body).map_err(|err| {
                            FunctionCallError::Fatal(format!(
                                "failed to parse team_broadcast send result: {err}"
                            ))
                        })?;
                    delivered.insert(recipient.name, parsed.submission_id);
                }
                Err(err) => {
                    failed.insert(recipient.name, format!("{err}"));
                }
            }
        }

        let content = serde_json::to_string(&TeamBroadcastResult {
            team_id: args.team_id,
            from: sender.name,
            delivered,
            failed: failed.clone(),
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize team_broadcast result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(failed.is_empty()),
        })
    }

    pub async fn wait_team(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: WaitTeamArgs = parse_arguments(&arguments)?;
        let team = team_snapshot(session.as_ref(), &args.team_id).await?;
        let include_lead = args.include_lead.unwrap_or(false);
        let member_filter = parse_member_filter(args.members)?;
        let mut members = team
            .members()
            .filter(|member| include_lead || member.thread_id != team.lead_thread_id)
            .filter(|member| {
                member_filter
                    .as_ref()
                    .is_none_or(|filter| filter.contains(&member.name.to_ascii_lowercase()))
            })
            .cloned()
            .collect::<Vec<_>>();
        members.sort_by(|left, right| left.name.cmp(&right.name));
        if members.is_empty() {
            return Err(FunctionCallError::RespondToModel(
                "wait_team has no members to wait on".to_string(),
            ));
        }

        let timeout_ms = args.timeout_ms.unwrap_or(DEFAULT_WAIT_TIMEOUT_MS);
        let timeout_ms = match timeout_ms {
            ms if ms <= 0 => {
                return Err(FunctionCallError::RespondToModel(
                    "timeout_ms must be greater than zero".to_owned(),
                ));
            }
            ms => ms.clamp(MIN_WAIT_TIMEOUT_MS, MAX_WAIT_TIMEOUT_MS),
        };

        let mut statuses = HashMap::<ThreadId, AgentStatus>::new();
        let timed_out = if args.wait_for_all {
            let deadline =
                tokio::time::Instant::now() + std::time::Duration::from_millis(timeout_ms as u64);
            let mut pending = members
                .iter()
                .map(|member| member.thread_id)
                .collect::<HashSet<_>>();

            for member in &members {
                let status = session
                    .services
                    .agent_control
                    .get_status(member.thread_id)
                    .await;
                if is_final(&status) {
                    statuses.insert(member.thread_id, status);
                    let _ = pending.remove(&member.thread_id);
                }
            }

            while !pending.is_empty() {
                let now = tokio::time::Instant::now();
                if now >= deadline {
                    break;
                }
                let remaining = deadline.saturating_duration_since(now);
                if remaining.is_zero() {
                    break;
                }
                let remaining_ms = (remaining.as_millis() as i64).max(1);
                if remaining_ms < MIN_WAIT_TIMEOUT_MS {
                    let pending_before = pending.clone();
                    let mut receiver_thread_ids =
                        pending_before.iter().copied().collect::<Vec<_>>();
                    receiver_thread_ids.sort_by_key(|thread_id| thread_id.to_string());
                    let mut receiver_agents = members
                        .iter()
                        .filter(|member| pending_before.contains(&member.thread_id))
                        .map(|member| CollabAgentRef {
                            thread_id: member.thread_id,
                            agent_nickname: member.nickname.clone(),
                            agent_role: member.role.clone(),
                        })
                        .collect::<Vec<_>>();
                    receiver_agents.sort_by_key(|agent| agent.thread_id.to_string());

                    session
                        .send_event(
                            &turn,
                            CollabWaitingBeginEvent {
                                sender_thread_id: session.conversation_id,
                                receiver_thread_ids,
                                receiver_agents: receiver_agents.clone(),
                                call_id: call_id.clone(),
                            }
                            .into(),
                        )
                        .await;

                    wait_for_pending_until_deadline(
                        session.clone(),
                        &mut pending,
                        &mut statuses,
                        deadline,
                    )
                    .await;

                    let end_statuses = pending_before
                        .into_iter()
                        .filter_map(|thread_id| {
                            statuses.get(&thread_id).and_then(|status| {
                                is_final(status).then_some((thread_id, status.clone()))
                            })
                        })
                        .collect::<HashMap<_, _>>();

                    session
                        .send_event(
                            &turn,
                            CollabWaitingEndEvent {
                                sender_thread_id: session.conversation_id,
                                call_id: call_id.clone(),
                                agent_statuses: build_wait_agent_statuses(
                                    &end_statuses,
                                    &receiver_agents,
                                ),
                                statuses: end_statuses,
                            }
                            .into(),
                        )
                        .await;

                    break;
                }
                let wait_timeout_ms = remaining_ms.min(MAX_WAIT_TIMEOUT_MS);
                let mut ids = pending.iter().map(ToString::to_string).collect::<Vec<_>>();
                ids.sort();
                let payload = json!({
                    "ids": ids,
                    "timeout_ms": wait_timeout_ms,
                });
                let wait_output = wait::handle(
                    session.clone(),
                    turn.clone(),
                    call_id.clone(),
                    payload.to_string(),
                )
                .await?;
                let body = tool_output_text(wait_output)?;
                let parsed: wait::WaitResult = serde_json::from_str(&body).map_err(|err| {
                    FunctionCallError::Fatal(format!("failed to parse wait_team result: {err}"))
                })?;
                for (thread_id, status) in parsed.status {
                    if is_final(&status) {
                        let _ = pending.remove(&thread_id);
                    }
                    statuses.insert(thread_id, status);
                }
                if parsed.timed_out {
                    break;
                }
            }

            for member in &members {
                if let std::collections::hash_map::Entry::Vacant(e) =
                    statuses.entry(member.thread_id)
                {
                    let status = session
                        .services
                        .agent_control
                        .get_status(member.thread_id)
                        .await;
                    e.insert(status.clone());
                    if is_final(&status) {
                        continue;
                    }
                }
            }

            members.iter().any(|member| {
                statuses
                    .get(&member.thread_id)
                    .is_none_or(|status| !is_final(status))
            })
        } else {
            let ids = members
                .iter()
                .map(|member| member.thread_id.to_string())
                .collect::<Vec<_>>();
            let payload = json!({
                "ids": ids,
                "timeout_ms": timeout_ms,
            });
            let wait_output = wait::handle(
                session.clone(),
                turn.clone(),
                call_id.clone(),
                payload.to_string(),
            )
            .await?;
            let body = tool_output_text(wait_output)?;
            let parsed: wait::WaitResult = serde_json::from_str(&body).map_err(|err| {
                FunctionCallError::Fatal(format!("failed to parse wait_team result: {err}"))
            })?;
            statuses = parsed.status;
            parsed.timed_out
        };

        for member in &members {
            if let std::collections::hash_map::Entry::Vacant(e) = statuses.entry(member.thread_id) {
                e.insert(
                    session
                        .services
                        .agent_control
                        .get_status(member.thread_id)
                        .await,
                );
            }
        }

        let mut status = BTreeMap::new();
        for member in &members {
            let member_status = statuses
                .get(&member.thread_id)
                .cloned()
                .unwrap_or(AgentStatus::NotFound);
            status.insert(member.name.clone(), member_status);
        }

        if (args.auto_cleanup || team.auto_cleanup) && !timed_out && args.wait_for_all {
            let cleanup_args = CloseTeamArgs {
                team_id: team.team_id.clone(),
                include_lead: Some(false),
                remove: true,
            };
            let serialized = serde_json::to_string(&cleanup_args).map_err(|err| {
                FunctionCallError::Fatal(format!(
                    "failed to serialize wait_team cleanup args: {err}"
                ))
            })?;
            let _ = close_team(session.clone(), turn.clone(), call_id.clone(), serialized).await?;
        }

        let content = serde_json::to_string(&WaitTeamResult {
            team_id: args.team_id,
            timed_out,
            status,
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize wait_team result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(!timed_out),
        })
    }

    pub async fn close_team(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: CloseTeamArgs = parse_arguments(&arguments)?;
        let team = team_snapshot(session.as_ref(), &args.team_id).await?;
        let include_lead = args.include_lead.unwrap_or(false);

        let mut members = team.members().cloned().collect::<Vec<_>>();
        members.sort_by(|left, right| left.name.cmp(&right.name));
        if !include_lead {
            members.retain(|member| member.thread_id != team.lead_thread_id);
        }

        let mut status = BTreeMap::new();
        for member in members {
            let current_status = session
                .services
                .agent_control
                .get_status(member.thread_id)
                .await;
            if matches!(
                current_status,
                AgentStatus::NotFound | AgentStatus::Shutdown
            ) {
                status.insert(member.name, current_status);
                continue;
            }
            let nested_call_id = format!("{call_id}:{}", member.name);
            let close_output = close_agent::handle(
                session.clone(),
                turn.clone(),
                nested_call_id,
                serde_json::to_string(&CloseAgentArgs {
                    id: member.thread_id.to_string(),
                })
                .map_err(|err| {
                    FunctionCallError::Fatal(format!("failed to serialize close_team args: {err}"))
                })?,
            )
            .await?;
            let body = tool_output_text(close_output)?;
            let parsed: close_agent::CloseAgentResult =
                serde_json::from_str(&body).map_err(|err| {
                    FunctionCallError::Fatal(format!(
                        "failed to parse close_team close_agent result: {err}"
                    ))
                })?;
            status.insert(member.name, parsed.status);
        }

        if args.remove {
            let team_registry = session.services.agent_control.team_registry();
            let mut registry = team_registry.lock().await;
            let _ = registry.remove_team(&args.team_id);
        }

        let content = serde_json::to_string(&CloseTeamResult {
            team_id: args.team_id,
            removed: args.remove,
            status,
        })
        .map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize close_team result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }

    pub async fn team_cleanup(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: TeamCleanupArgs = parse_arguments(&arguments)?;
        let close_args = CloseTeamArgs {
            team_id: args.team_id,
            include_lead: args.include_lead,
            remove: true,
        };
        let serialized = serde_json::to_string(&close_args).map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize team_cleanup args: {err}"))
        })?;
        close_team(session, turn, call_id, serialized).await
    }

    async fn wait_for_pending_until_deadline(
        session: Arc<Session>,
        pending: &mut HashSet<ThreadId>,
        statuses: &mut HashMap<ThreadId, AgentStatus>,
        deadline: tokio::time::Instant,
    ) {
        let mut waiters = FuturesUnordered::new();
        let pending_ids = pending.iter().copied().collect::<Vec<_>>();
        for thread_id in pending_ids {
            match session
                .services
                .agent_control
                .subscribe_status(thread_id)
                .await
            {
                Ok(rx) => {
                    waiters.push(wait::wait_for_final_status(session.clone(), thread_id, rx));
                }
                Err(CodexErr::ThreadNotFound(_)) => {
                    statuses.insert(thread_id, AgentStatus::NotFound);
                    let _ = pending.remove(&thread_id);
                }
                Err(_) => {
                    let status = session.services.agent_control.get_status(thread_id).await;
                    if is_final(&status) {
                        let _ = pending.remove(&thread_id);
                    }
                    statuses.insert(thread_id, status);
                }
            }
        }

        while !pending.is_empty() && !waiters.is_empty() {
            match tokio::time::timeout_at(deadline, waiters.next()).await {
                Ok(Some(Some((thread_id, status)))) => {
                    statuses.insert(thread_id, status.clone());
                    if is_final(&status) {
                        let _ = pending.remove(&thread_id);
                    }
                }
                Ok(Some(None)) => continue,
                Ok(None) | Err(_) => break,
            }
        }
    }

    async fn team_snapshot(
        session: &Session,
        team_id: &str,
    ) -> Result<TeamState, FunctionCallError> {
        let team_registry = session.services.agent_control.team_registry();
        let registry = team_registry.lock().await;
        registry
            .team(team_id)
            .ok_or_else(|| FunctionCallError::RespondToModel(format!("team not found: {team_id}")))
    }

    fn resolve_sender(
        team: &TeamState,
        caller_thread_id: ThreadId,
        requested_sender: Option<&str>,
        tool_name: &str,
    ) -> Result<TeamMember, FunctionCallError> {
        let sender = if let Some(requested_sender) = requested_sender {
            team.member(requested_sender).cloned().ok_or_else(|| {
                FunctionCallError::RespondToModel(format!(
                    "{tool_name} sender not found in team {}: {requested_sender}",
                    team.team_id
                ))
            })?
        } else {
            team.member_by_thread_id(caller_thread_id)
                .cloned()
                .ok_or_else(|| {
                    FunctionCallError::RespondToModel(format!(
                        "{tool_name} caller thread is not a member of team {}",
                        team.team_id
                    ))
                })?
        };

        if caller_thread_id != team.lead_thread_id && caller_thread_id != sender.thread_id {
            return Err(FunctionCallError::RespondToModel(format!(
                "{tool_name} caller is not authorized to send as {}",
                sender.name
            )));
        }

        Ok(sender)
    }

    fn parse_member_filter(
        members: Option<Vec<String>>,
    ) -> Result<Option<HashSet<String>>, FunctionCallError> {
        let Some(members) = members else {
            return Ok(None);
        };
        if members.is_empty() {
            return Err(FunctionCallError::RespondToModel(
                "members can't be empty when provided".to_string(),
            ));
        }
        let mut filter = HashSet::new();
        for member in members {
            let trimmed = member.trim();
            if trimmed.is_empty() {
                return Err(FunctionCallError::RespondToModel(
                    "members can't include empty values".to_string(),
                ));
            }
            filter.insert(trimmed.to_ascii_lowercase());
        }
        Ok(Some(filter))
    }

    async fn shutdown_spawned_agents(session: &Session, thread_ids: &[ThreadId]) {
        for thread_id in thread_ids {
            let _ = session
                .services
                .agent_control
                .shutdown_agent(*thread_id)
                .await;
        }
    }

    fn tool_output_text(output: ToolOutput) -> Result<String, FunctionCallError> {
        match output {
            ToolOutput::Function {
                body: FunctionCallOutputBody::Text(content),
                ..
            } => Ok(content),
            _ => Err(FunctionCallError::Fatal(
                "expected function text output".to_string(),
            )),
        }
    }
}

mod send_input {
    use super::*;
    use std::sync::Arc;

    #[derive(Debug, Deserialize)]
    struct SendInputArgs {
        id: String,
        message: Option<String>,
        items: Option<Vec<UserInput>>,
        #[serde(default)]
        interrupt: bool,
    }

    #[derive(Debug, Serialize)]
    struct SendInputResult {
        submission_id: String,
    }

    pub async fn handle(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: SendInputArgs = parse_arguments(&arguments)?;
        let receiver_thread_id = agent_id(&args.id)?;
        let input_items = parse_collab_input(args.message, args.items)?;
        let prompt = input_preview(&input_items);
        let (receiver_agent_nickname, receiver_agent_role) = session
            .services
            .agent_control
            .get_agent_nickname_and_role(receiver_thread_id)
            .await
            .unwrap_or((None, None));
        if args.interrupt {
            session
                .services
                .agent_control
                .interrupt_agent(receiver_thread_id)
                .await
                .map_err(|err| collab_agent_error(receiver_thread_id, err))?;
        }
        session
            .send_event(
                &turn,
                CollabAgentInteractionBeginEvent {
                    call_id: call_id.clone(),
                    sender_thread_id: session.conversation_id,
                    receiver_thread_id,
                    prompt: prompt.clone(),
                }
                .into(),
            )
            .await;
        let result = session
            .services
            .agent_control
            .send_input(receiver_thread_id, input_items)
            .await
            .map_err(|err| collab_agent_error(receiver_thread_id, err));
        let status = session
            .services
            .agent_control
            .get_status(receiver_thread_id)
            .await;
        session
            .send_event(
                &turn,
                CollabAgentInteractionEndEvent {
                    call_id,
                    sender_thread_id: session.conversation_id,
                    receiver_thread_id,
                    receiver_agent_nickname,
                    receiver_agent_role,
                    prompt,
                    status,
                }
                .into(),
            )
            .await;
        let submission_id = result?;

        let content = serde_json::to_string(&SendInputResult { submission_id }).map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize send_input result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }
}

mod resume_agent {
    use super::*;
    use crate::agent::next_thread_spawn_depth;
    use std::sync::Arc;

    #[derive(Debug, Deserialize)]
    struct ResumeAgentArgs {
        id: String,
    }

    #[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
    pub(super) struct ResumeAgentResult {
        pub(super) status: AgentStatus,
    }

    pub async fn handle(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: ResumeAgentArgs = parse_arguments(&arguments)?;
        let receiver_thread_id = agent_id(&args.id)?;
        let (receiver_agent_nickname, receiver_agent_role) = session
            .services
            .agent_control
            .get_agent_nickname_and_role(receiver_thread_id)
            .await
            .unwrap_or((None, None));
        let child_depth = next_thread_spawn_depth(&turn.session_source);
        let max_depth = turn.config.agent_max_depth;
        if exceeds_thread_spawn_depth_limit(child_depth, max_depth) {
            return Err(FunctionCallError::RespondToModel(
                "Agent depth limit reached. Solve the task yourself.".to_string(),
            ));
        }

        session
            .send_event(
                &turn,
                CollabResumeBeginEvent {
                    call_id: call_id.clone(),
                    sender_thread_id: session.conversation_id,
                    receiver_thread_id,
                    receiver_agent_nickname: receiver_agent_nickname.clone(),
                    receiver_agent_role: receiver_agent_role.clone(),
                }
                .into(),
            )
            .await;

        let mut status = session
            .services
            .agent_control
            .get_status(receiver_thread_id)
            .await;
        let error = if matches!(status, AgentStatus::NotFound) {
            // If the thread is no longer active, attempt to restore it from rollout.
            match try_resume_closed_agent(&session, &turn, receiver_thread_id, child_depth).await {
                Ok(resumed_status) => {
                    status = resumed_status;
                    None
                }
                Err(err) => {
                    status = session
                        .services
                        .agent_control
                        .get_status(receiver_thread_id)
                        .await;
                    Some(err)
                }
            }
        } else {
            None
        };

        let (receiver_agent_nickname, receiver_agent_role) = session
            .services
            .agent_control
            .get_agent_nickname_and_role(receiver_thread_id)
            .await
            .unwrap_or((receiver_agent_nickname, receiver_agent_role));
        session
            .send_event(
                &turn,
                CollabResumeEndEvent {
                    call_id,
                    sender_thread_id: session.conversation_id,
                    receiver_thread_id,
                    receiver_agent_nickname,
                    receiver_agent_role,
                    status: status.clone(),
                }
                .into(),
            )
            .await;

        if let Some(err) = error {
            return Err(err);
        }
        turn.otel_manager
            .counter("codex.multi_agent.resume", 1, &[]);

        let content = serde_json::to_string(&ResumeAgentResult { status }).map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize resume_agent result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }

    async fn try_resume_closed_agent(
        session: &Arc<Session>,
        turn: &Arc<TurnContext>,
        receiver_thread_id: ThreadId,
        child_depth: i32,
    ) -> Result<AgentStatus, FunctionCallError> {
        let config = build_agent_resume_config(turn.as_ref(), child_depth)?;
        let resumed_thread_id = session
            .services
            .agent_control
            .resume_agent_from_rollout(
                config,
                receiver_thread_id,
                thread_spawn_source(session.conversation_id, child_depth, None),
            )
            .await
            .map_err(|err| collab_agent_error(receiver_thread_id, err))?;

        Ok(session
            .services
            .agent_control
            .get_status(resumed_thread_id)
            .await)
    }
}

pub(crate) mod wait {
    use super::*;
    use crate::agent::status::is_final;
    use futures::FutureExt;
    use futures::StreamExt;
    use futures::stream::FuturesUnordered;
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::sync::watch::Receiver;
    use tokio::time::Instant;

    use tokio::time::timeout_at;

    #[derive(Debug, Deserialize)]
    struct WaitArgs {
        ids: Vec<String>,
        timeout_ms: Option<i64>,
    }

    #[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
    pub(crate) struct WaitResult {
        pub(crate) status: HashMap<ThreadId, AgentStatus>,
        pub(crate) timed_out: bool,
    }

    pub async fn handle(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: WaitArgs = parse_arguments(&arguments)?;
        if args.ids.is_empty() {
            return Err(FunctionCallError::RespondToModel(
                "ids must be non-empty".to_owned(),
            ));
        }
        let receiver_thread_ids = args
            .ids
            .iter()
            .map(|id| agent_id(id))
            .collect::<Result<Vec<_>, _>>()?;
        let mut receiver_agents = Vec::with_capacity(receiver_thread_ids.len());
        for receiver_thread_id in &receiver_thread_ids {
            let (agent_nickname, agent_role) = session
                .services
                .agent_control
                .get_agent_nickname_and_role(*receiver_thread_id)
                .await
                .unwrap_or((None, None));
            receiver_agents.push(CollabAgentRef {
                thread_id: *receiver_thread_id,
                agent_nickname,
                agent_role,
            });
        }

        // Validate timeout.
        // Very short timeouts encourage busy-polling loops in the orchestrator prompt and can
        // cause high CPU usage even with a single active worker, so clamp to a minimum.
        let timeout_ms = args.timeout_ms.unwrap_or(DEFAULT_WAIT_TIMEOUT_MS);
        let timeout_ms = match timeout_ms {
            ms if ms <= 0 => {
                return Err(FunctionCallError::RespondToModel(
                    "timeout_ms must be greater than zero".to_owned(),
                ));
            }
            ms => ms.clamp(MIN_WAIT_TIMEOUT_MS, MAX_WAIT_TIMEOUT_MS),
        };
        let deadline = Instant::now() + Duration::from_millis(timeout_ms as u64);

        session
            .send_event(
                &turn,
                CollabWaitingBeginEvent {
                    sender_thread_id: session.conversation_id,
                    receiver_thread_ids: receiver_thread_ids.clone(),
                    receiver_agents: receiver_agents.clone(),
                    call_id: call_id.clone(),
                }
                .into(),
            )
            .await;

        let mut status_rxs = Vec::with_capacity(receiver_thread_ids.len());
        let mut initial_final_statuses = Vec::new();
        for id in &receiver_thread_ids {
            match session.services.agent_control.subscribe_status(*id).await {
                Ok(rx) => {
                    let status = rx.borrow().clone();
                    if is_final(&status) {
                        initial_final_statuses.push((*id, status));
                    }
                    status_rxs.push((*id, rx));
                }
                Err(CodexErr::ThreadNotFound(_)) => {
                    initial_final_statuses.push((*id, AgentStatus::NotFound));
                }
                Err(err) => {
                    let mut statuses = HashMap::with_capacity(1);
                    statuses.insert(*id, session.services.agent_control.get_status(*id).await);
                    session
                        .send_event(
                            &turn,
                            CollabWaitingEndEvent {
                                sender_thread_id: session.conversation_id,
                                call_id: call_id.clone(),
                                agent_statuses: build_wait_agent_statuses(
                                    &statuses,
                                    &receiver_agents,
                                ),
                                statuses,
                            }
                            .into(),
                        )
                        .await;
                    return Err(collab_agent_error(*id, err));
                }
            }
        }

        let statuses = if !initial_final_statuses.is_empty() {
            initial_final_statuses
        } else {
            // Wait for the first agent to reach a final status.
            let mut futures = FuturesUnordered::new();
            for (id, rx) in status_rxs.into_iter() {
                let session = session.clone();
                futures.push(wait_for_final_status(session, id, rx));
            }
            let mut results = Vec::new();
            loop {
                match timeout_at(deadline, futures.next()).await {
                    Ok(Some(Some(result))) => {
                        results.push(result);
                        break;
                    }
                    Ok(Some(None)) => continue,
                    Ok(None) | Err(_) => break,
                }
            }
            if !results.is_empty() {
                // Drain the unlikely last elements to prevent race.
                loop {
                    match futures.next().now_or_never() {
                        Some(Some(Some(result))) => results.push(result),
                        Some(Some(None)) => continue,
                        Some(None) | None => break,
                    }
                }
            }
            results
        };

        // Convert payload.
        let statuses_map = statuses.clone().into_iter().collect::<HashMap<_, _>>();
        let agent_statuses = build_wait_agent_statuses(&statuses_map, &receiver_agents);
        let result = WaitResult {
            status: statuses_map.clone(),
            timed_out: statuses.is_empty(),
        };

        // Final event emission.
        session
            .send_event(
                &turn,
                CollabWaitingEndEvent {
                    sender_thread_id: session.conversation_id,
                    call_id,
                    agent_statuses,
                    statuses: statuses_map,
                }
                .into(),
            )
            .await;

        let content = serde_json::to_string(&result).map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize wait result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: None,
        })
    }

    pub(super) async fn wait_for_final_status(
        session: Arc<Session>,
        thread_id: ThreadId,
        mut status_rx: Receiver<AgentStatus>,
    ) -> Option<(ThreadId, AgentStatus)> {
        let mut status = status_rx.borrow().clone();
        if is_final(&status) {
            return Some((thread_id, status));
        }

        loop {
            if status_rx.changed().await.is_err() {
                let latest = session.services.agent_control.get_status(thread_id).await;
                return is_final(&latest).then_some((thread_id, latest));
            }
            status = status_rx.borrow().clone();
            if is_final(&status) {
                return Some((thread_id, status));
            }
        }
    }
}

pub mod close_agent {
    use super::*;
    use std::sync::Arc;

    #[derive(Debug, Deserialize, Serialize)]
    pub(super) struct CloseAgentResult {
        pub(super) status: AgentStatus,
    }

    pub async fn handle(
        session: Arc<Session>,
        turn: Arc<TurnContext>,
        call_id: String,
        arguments: String,
    ) -> Result<ToolOutput, FunctionCallError> {
        let args: CloseAgentArgs = parse_arguments(&arguments)?;
        let agent_id = agent_id(&args.id)?;
        let (receiver_agent_nickname, receiver_agent_role) = session
            .services
            .agent_control
            .get_agent_nickname_and_role(agent_id)
            .await
            .unwrap_or((None, None));
        session
            .send_event(
                &turn,
                CollabCloseBeginEvent {
                    call_id: call_id.clone(),
                    sender_thread_id: session.conversation_id,
                    receiver_thread_id: agent_id,
                }
                .into(),
            )
            .await;
        let status = match session
            .services
            .agent_control
            .subscribe_status(agent_id)
            .await
        {
            Ok(mut status_rx) => status_rx.borrow_and_update().clone(),
            Err(err) => {
                let status = session.services.agent_control.get_status(agent_id).await;
                session
                    .send_event(
                        &turn,
                        CollabCloseEndEvent {
                            call_id: call_id.clone(),
                            sender_thread_id: session.conversation_id,
                            receiver_thread_id: agent_id,
                            receiver_agent_nickname: receiver_agent_nickname.clone(),
                            receiver_agent_role: receiver_agent_role.clone(),
                            status,
                        }
                        .into(),
                    )
                    .await;
                return Err(collab_agent_error(agent_id, err));
            }
        };
        let result = if !matches!(status, AgentStatus::Shutdown) {
            session
                .services
                .agent_control
                .shutdown_agent(agent_id)
                .await
                .map_err(|err| collab_agent_error(agent_id, err))
                .map(|_| ())
        } else {
            Ok(())
        };
        session
            .send_event(
                &turn,
                CollabCloseEndEvent {
                    call_id,
                    sender_thread_id: session.conversation_id,
                    receiver_thread_id: agent_id,
                    receiver_agent_nickname,
                    receiver_agent_role,
                    status: status.clone(),
                }
                .into(),
            )
            .await;
        result?;

        let content = serde_json::to_string(&CloseAgentResult { status }).map_err(|err| {
            FunctionCallError::Fatal(format!("failed to serialize close_agent result: {err}"))
        })?;

        Ok(ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success: Some(true),
        })
    }
}

fn agent_id(id: &str) -> Result<ThreadId, FunctionCallError> {
    ThreadId::from_string(id)
        .map_err(|e| FunctionCallError::RespondToModel(format!("invalid agent id {id}: {e:?}")))
}

fn build_wait_agent_statuses(
    statuses: &HashMap<ThreadId, AgentStatus>,
    receiver_agents: &[CollabAgentRef],
) -> Vec<CollabAgentStatusEntry> {
    if statuses.is_empty() {
        return Vec::new();
    }

    let mut entries = Vec::with_capacity(statuses.len());
    let mut seen = HashMap::with_capacity(receiver_agents.len());
    for receiver_agent in receiver_agents {
        seen.insert(receiver_agent.thread_id, ());
        if let Some(status) = statuses.get(&receiver_agent.thread_id) {
            entries.push(CollabAgentStatusEntry {
                thread_id: receiver_agent.thread_id,
                agent_nickname: receiver_agent.agent_nickname.clone(),
                agent_role: receiver_agent.agent_role.clone(),
                status: status.clone(),
            });
        }
    }

    let mut extras = statuses
        .iter()
        .filter(|(thread_id, _)| !seen.contains_key(thread_id))
        .map(|(thread_id, status)| CollabAgentStatusEntry {
            thread_id: *thread_id,
            agent_nickname: None,
            agent_role: None,
            status: status.clone(),
        })
        .collect::<Vec<_>>();
    extras.sort_by(|left, right| left.thread_id.to_string().cmp(&right.thread_id.to_string()));
    entries.extend(extras);
    entries
}

fn collab_spawn_error(err: CodexErr) -> FunctionCallError {
    match err {
        CodexErr::UnsupportedOperation(_) => {
            FunctionCallError::RespondToModel("collab manager unavailable".to_string())
        }
        err => FunctionCallError::RespondToModel(format!("collab spawn failed: {err}")),
    }
}

fn collab_agent_error(agent_id: ThreadId, err: CodexErr) -> FunctionCallError {
    match err {
        CodexErr::ThreadNotFound(id) => {
            FunctionCallError::RespondToModel(format!("agent with id {id} not found"))
        }
        CodexErr::InternalAgentDied => {
            FunctionCallError::RespondToModel(format!("agent with id {agent_id} is closed"))
        }
        CodexErr::UnsupportedOperation(_) => {
            FunctionCallError::RespondToModel("collab manager unavailable".to_string())
        }
        err => FunctionCallError::RespondToModel(format!("collab tool failed: {err}")),
    }
}

fn thread_spawn_source(
    parent_thread_id: ThreadId,
    depth: i32,
    agent_role: Option<&str>,
) -> SessionSource {
    SessionSource::SubAgent(SubAgentSource::ThreadSpawn {
        parent_thread_id,
        depth,
        agent_nickname: None,
        agent_role: agent_role.map(str::to_string),
    })
}

fn parse_collab_input(
    message: Option<String>,
    items: Option<Vec<UserInput>>,
) -> Result<Vec<UserInput>, FunctionCallError> {
    match (message, items) {
        (Some(_), Some(_)) => Err(FunctionCallError::RespondToModel(
            "Provide either message or items, but not both".to_string(),
        )),
        (None, None) => Err(FunctionCallError::RespondToModel(
            "Provide one of: message or items".to_string(),
        )),
        (Some(message), None) => {
            if message.trim().is_empty() {
                return Err(FunctionCallError::RespondToModel(
                    "Empty message can't be sent to an agent".to_string(),
                ));
            }
            Ok(vec![UserInput::Text {
                text: message,
                text_elements: Vec::new(),
            }])
        }
        (None, Some(items)) => {
            if items.is_empty() {
                return Err(FunctionCallError::RespondToModel(
                    "Items can't be empty".to_string(),
                ));
            }
            Ok(items)
        }
    }
}

fn input_preview(items: &[UserInput]) -> String {
    let parts: Vec<String> = items
        .iter()
        .map(|item| match item {
            UserInput::Text { text, .. } => text.clone(),
            UserInput::Image { .. } => "[image]".to_string(),
            UserInput::LocalImage { path } => format!("[local_image:{}]", path.display()),
            UserInput::Skill { name, path } => {
                format!("[skill:${name}]({})", path.display())
            }
            UserInput::Mention { name, path } => format!("[mention:${name}]({path})"),
            _ => "[input]".to_string(),
        })
        .collect();

    parts.join("\n")
}

pub(crate) fn build_agent_spawn_config(
    base_instructions: &BaseInstructions,
    turn: &TurnContext,
) -> Result<Config, FunctionCallError> {
    let mut config = build_agent_shared_config(turn)?;
    config.base_instructions = Some(base_instructions.text.clone());
    Ok(config)
}

fn build_agent_resume_config(
    turn: &TurnContext,
    child_depth: i32,
) -> Result<Config, FunctionCallError> {
    let mut config = build_agent_shared_config(turn)?;
    apply_spawn_agent_overrides(&mut config, child_depth);
    // For resume, keep base instructions sourced from rollout/session metadata.
    config.base_instructions = None;
    Ok(config)
}

fn build_agent_shared_config(turn: &TurnContext) -> Result<Config, FunctionCallError> {
    let base_config = turn.config.clone();
    let mut config = (*base_config).clone();
    config.model = Some(turn.model_info.slug.clone());
    config.model_provider = turn.provider.clone();
    config.model_reasoning_effort = turn.reasoning_effort;
    config.model_reasoning_summary = Some(turn.reasoning_summary);
    config.developer_instructions = turn.developer_instructions.clone();
    config.compact_prompt = turn.compact_prompt.clone();
    apply_spawn_agent_runtime_overrides(&mut config, turn)?;

    Ok(config)
}

fn apply_spawn_agent_runtime_overrides(
    config: &mut Config,
    turn: &TurnContext,
) -> Result<(), FunctionCallError> {
    config
        .permissions
        .approval_policy
        .set(turn.approval_policy.value())
        .map_err(|err| {
            FunctionCallError::RespondToModel(format!("approval_policy is invalid: {err}"))
        })?;
    config.permissions.shell_environment_policy = turn.shell_environment_policy.clone();
    config.codex_linux_sandbox_exe = turn.codex_linux_sandbox_exe.clone();
    config.cwd = turn.cwd.clone();
    config
        .permissions
        .sandbox_policy
        .set(turn.sandbox_policy.get().clone())
        .map_err(|err| {
            FunctionCallError::RespondToModel(format!("sandbox_policy is invalid: {err}"))
        })?;
    Ok(())
}

fn apply_spawn_agent_overrides(config: &mut Config, child_depth: i32) {
    if child_depth >= config.agent_max_depth {
        config.features.disable(Feature::Collab);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AuthManager;
    use crate::CodexAuth;
    use crate::ThreadManager;
    use crate::built_in_model_providers;
    use crate::codex::make_session_and_context;
    use crate::config::DEFAULT_AGENT_MAX_DEPTH;
    use crate::config::types::ShellEnvironmentPolicy;
    use crate::function_tool::FunctionCallError;
    use crate::protocol::AskForApproval;
    use crate::protocol::Op;
    use crate::protocol::SandboxPolicy;
    use crate::protocol::SessionSource;
    use crate::protocol::SubAgentSource;
    use crate::turn_diff_tracker::TurnDiffTracker;
    use codex_protocol::ThreadId;
    use codex_protocol::models::ContentItem;
    use codex_protocol::models::ResponseItem;
    use codex_protocol::protocol::InitialHistory;
    use codex_protocol::protocol::RolloutItem;
    use pretty_assertions::assert_eq;
    use serde::Deserialize;
    use serde_json::json;
    use std::collections::HashMap;
    use std::path::PathBuf;
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::sync::Mutex;
    use tokio::time::timeout;

    fn invocation(
        session: Arc<crate::codex::Session>,
        turn: Arc<TurnContext>,
        tool_name: &str,
        payload: ToolPayload,
    ) -> ToolInvocation {
        ToolInvocation {
            session,
            turn,
            tracker: Arc::new(Mutex::new(TurnDiffTracker::default())),
            call_id: "call-1".to_string(),
            tool_name: tool_name.to_string(),
            payload,
        }
    }

    fn function_payload(args: serde_json::Value) -> ToolPayload {
        ToolPayload::Function {
            arguments: args.to_string(),
        }
    }

    fn thread_manager() -> ThreadManager {
        ThreadManager::with_models_provider_for_tests(
            CodexAuth::from_api_key("dummy"),
            built_in_model_providers()["openai"].clone(),
        )
    }

    async fn register_team(
        session: &crate::codex::Session,
        team_name: &str,
        lead_name: &str,
        workers: Vec<TeamMember>,
        auto_cleanup: bool,
    ) -> String {
        let team_registry = session.services.agent_control.team_registry();
        let mut registry = team_registry.lock().await;
        registry
            .create_team(
                Some(team_name),
                TeamMember {
                    name: lead_name.to_string(),
                    thread_id: session.conversation_id,
                    nickname: None,
                    role: Some("team-lead".to_string()),
                },
                workers,
                auto_cleanup,
            )
            .expect("team should be created")
            .team_id
    }

    #[tokio::test]
    async fn handler_rejects_non_function_payloads() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            ToolPayload::Custom {
                input: "hello".to_string(),
            },
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("payload should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(
                "collab handler received unsupported payload".to_string()
            )
        );
    }

    #[tokio::test]
    async fn handler_rejects_unknown_tool() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "unknown_tool",
            function_payload(json!({})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("tool should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("unsupported collab tool unknown_tool".to_string())
        );
    }

    #[tokio::test]
    async fn spawn_team_requires_agents() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_team",
            function_payload(json!({"agents": []})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("spawn_team should reject empty agents");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("agents must be non-empty".to_string())
        );
    }

    #[tokio::test]
    async fn team_message_rejects_unknown_team() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "team_message",
            function_payload(json!({
                "team_id": "team-missing",
                "to": "worker",
                "message": "hello"
            })),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("team_message should reject missing team");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("team not found: team-missing".to_string())
        );
    }

    #[tokio::test]
    async fn wait_team_rejects_unknown_team() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait_team",
            function_payload(json!({
                "team_id": "team-missing"
            })),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("wait_team should reject missing team");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("team not found: team-missing".to_string())
        );
    }

    #[tokio::test]
    async fn wait_team_resolves_shared_registry_from_member_session() {
        let (mut lead_session, _lead_turn) = make_session_and_context().await;
        let (mut member_session, member_turn) = make_session_and_context().await;
        lead_session.conversation_id = ThreadId::new();
        member_session.conversation_id = ThreadId::new();
        let manager = thread_manager();
        let shared_control = manager.agent_control();
        lead_session.services.agent_control = shared_control.clone();
        member_session.services.agent_control = shared_control;

        let team_id = {
            let team_registry = lead_session.services.agent_control.team_registry();
            let mut registry = team_registry.lock().await;
            registry
                .create_team(
                    Some("alpha"),
                    TeamMember {
                        name: "lead".to_string(),
                        thread_id: lead_session.conversation_id,
                        nickname: None,
                        role: Some("team-lead".to_string()),
                    },
                    vec![TeamMember {
                        name: "worker".to_string(),
                        thread_id: member_session.conversation_id,
                        nickname: None,
                        role: Some("runtime-agent".to_string()),
                    }],
                    false,
                )
                .expect("team should be created")
                .team_id
        };

        let invocation = invocation(
            Arc::new(member_session),
            Arc::new(member_turn),
            "wait_team",
            function_payload(json!({
                "team_id": team_id.clone(),
                "timeout_ms": MIN_WAIT_TIMEOUT_MS
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("member session should resolve shared team registry");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("wait_team should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("wait_team output should be JSON");
        assert_eq!(parsed["team_id"], json!(team_id));
    }

    #[tokio::test]
    async fn list_teams_returns_sorted_team_summaries() {
        let (session, turn) = make_session_and_context().await;
        let alpha_worker = TeamMember {
            name: "worker".to_string(),
            thread_id: ThreadId::new(),
            nickname: None,
            role: Some("runtime-agent".to_string()),
        };
        let _alpha =
            register_team(&session, "alpha", "lead-alpha", vec![alpha_worker], false).await;
        let _beta = register_team(&session, "beta", "lead-beta", vec![], true).await;

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "list_teams",
            function_payload(json!({})),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("list_teams should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("list_teams should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("list_teams output should be JSON");
        let teams = parsed["teams"]
            .as_array()
            .expect("list_teams result should contain teams");
        assert_eq!(teams.len(), 2);
        assert_eq!(teams[0]["team_name"], json!("alpha"));
        assert_eq!(teams[1]["team_name"], json!("beta"));
        assert_eq!(teams[0]["lead_name"], json!("lead-alpha"));
        assert_eq!(teams[0]["member_count"], json!(2));
        assert_eq!(teams[0]["auto_cleanup"], json!(false));
        assert_eq!(teams[1]["member_count"], json!(1));
        assert_eq!(teams[1]["auto_cleanup"], json!(true));
    }

    #[tokio::test]
    async fn get_team_returns_member_mapping() {
        let (session, turn) = make_session_and_context().await;
        let worker = TeamMember {
            name: "worker".to_string(),
            thread_id: ThreadId::new(),
            nickname: Some("Worker".to_string()),
            role: Some("explorer".to_string()),
        };
        let team_id = register_team(&session, "alpha", "lead", vec![worker], true).await;

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "get_team",
            function_payload(json!({
                "team_id": team_id,
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("get_team should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("get_team should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("get_team output should be JSON");
        assert_eq!(parsed["team_name"], json!("alpha"));
        assert_eq!(parsed["lead_name"], json!("lead"));
        assert_eq!(parsed["auto_cleanup"], json!(true));
        assert_eq!(parsed["members"][0]["name"], json!("lead"));
        assert_eq!(parsed["members"][1]["name"], json!("worker"));
    }

    #[tokio::test]
    async fn team_member_status_requires_non_empty_target_set() {
        let (session, turn) = make_session_and_context().await;
        let team_id = register_team(&session, "alpha", "lead", vec![], false).await;

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "team_member_status",
            function_payload(json!({
                "team_id": team_id,
                "members": [],
            })),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("team_member_status should reject empty members filter");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("members can't be empty when provided".to_string())
        );
    }

    #[tokio::test]
    async fn wait_team_auto_cleanup_removes_team_after_completion() {
        let (session, turn) = make_session_and_context().await;
        let worker = TeamMember {
            name: "worker".to_string(),
            thread_id: ThreadId::new(),
            nickname: None,
            role: Some("runtime-agent".to_string()),
        };
        let team_id = register_team(&session, "alpha", "lead", vec![worker], true).await;
        let session = Arc::new(session);
        let turn = Arc::new(turn);

        let invocation = invocation(
            session.clone(),
            turn.clone(),
            "wait_team",
            function_payload(json!({
                "team_id": team_id.clone(),
                "wait_for_all": true,
                "timeout_ms": MIN_WAIT_TIMEOUT_MS,
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("wait_team should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("wait_team should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("wait_team output should be JSON");
        assert_eq!(parsed["timed_out"], json!(false));
        assert_eq!(parsed["status"]["worker"], json!("not_found"));

        let team_registry = session.services.agent_control.team_registry();
        let registry = team_registry.lock().await;
        assert_eq!(registry.team(&team_id), None);
    }

    #[tokio::test]
    async fn wait_team_preserves_live_status_for_non_final_members() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let running = manager.start_thread(config).await.expect("start thread");

        let team_id = register_team(
            &session,
            "alpha",
            "lead",
            vec![TeamMember {
                name: "worker".to_string(),
                thread_id: running.thread_id,
                nickname: None,
                role: Some("runtime-agent".to_string()),
            }],
            false,
        )
        .await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait_team",
            function_payload(json!({
                "team_id": team_id,
                "timeout_ms": MIN_WAIT_TIMEOUT_MS,
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("wait_team should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("wait_team should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("wait_team output should be JSON");
        assert_eq!(parsed["timed_out"], json!(true));
        assert_ne!(parsed["status"]["worker"], json!("not_found"));

        let _ = running.thread.submit(Op::Shutdown {}).await;
    }

    #[tokio::test]
    async fn wait_team_wait_for_all_respects_caller_timeout() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let running = manager
            .start_thread(config.clone())
            .await
            .expect("start running thread");
        let finisher = manager
            .start_thread(config)
            .await
            .expect("start finisher thread");

        let agent_control = manager.agent_control();
        let finisher_id = finisher.thread_id;
        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_millis(2_500)).await;
            let _ = agent_control.shutdown_agent(finisher_id).await;
        });

        let team_id = register_team(
            &session,
            "alpha",
            "lead",
            vec![
                TeamMember {
                    name: "running".to_string(),
                    thread_id: running.thread_id,
                    nickname: None,
                    role: Some("runtime-agent".to_string()),
                },
                TeamMember {
                    name: "finisher".to_string(),
                    thread_id: finisher.thread_id,
                    nickname: None,
                    role: Some("runtime-agent".to_string()),
                },
            ],
            false,
        )
        .await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait_team",
            function_payload(json!({
                "team_id": team_id,
                "wait_for_all": true,
                "timeout_ms": 11_000,
            })),
        );

        let start = tokio::time::Instant::now();
        let waited = timeout(
            Duration::from_millis(12_200),
            MultiAgentHandler.handle(invocation),
        )
        .await;
        assert!(
            waited.is_ok(),
            "wait_team exceeded requested timeout budget"
        );
        let output = waited
            .expect("timeout should not elapse")
            .expect("wait_team should succeed");
        let elapsed = start.elapsed();
        assert!(
            elapsed >= Duration::from_millis(10_500),
            "wait_team returned before caller timeout budget elapsed: {elapsed:?}"
        );
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("wait_team should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("wait_team output should be JSON");
        assert_eq!(parsed["timed_out"], json!(true));

        let _ = running.thread.submit(Op::Shutdown {}).await;
        let _ = finisher.thread.submit(Op::Shutdown {}).await;
    }

    #[tokio::test]
    async fn close_team_is_idempotent_when_members_are_already_closed() {
        let (session, turn) = make_session_and_context().await;
        let worker = TeamMember {
            name: "worker".to_string(),
            thread_id: ThreadId::new(),
            nickname: None,
            role: Some("runtime-agent".to_string()),
        };
        let team_id = register_team(&session, "alpha", "lead", vec![worker], false).await;
        let session = Arc::new(session);
        let turn = Arc::new(turn);

        let first_close = invocation(
            session.clone(),
            turn.clone(),
            "close_team",
            function_payload(json!({
                "team_id": team_id.clone(),
                "remove": false,
            })),
        );
        let output = MultiAgentHandler
            .handle(first_close)
            .await
            .expect("close_team should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("close_team should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("close_team output should be JSON");
        assert_eq!(parsed["status"]["worker"], json!("not_found"));

        let second_close = invocation(
            session.clone(),
            turn,
            "close_team",
            function_payload(json!({
                "team_id": team_id.clone(),
                "remove": true,
            })),
        );
        let output = MultiAgentHandler
            .handle(second_close)
            .await
            .expect("close_team should remain idempotent");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(body),
            ..
        } = output
        else {
            panic!("close_team should return text output");
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&body).expect("close_team output should be JSON");
        assert_eq!(parsed["removed"], json!(true));
        assert_eq!(parsed["status"]["worker"], json!("not_found"));
    }

    #[tokio::test]
    async fn spawn_agent_rejects_empty_message() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            function_payload(json!({"message": "   "})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("empty message should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(
                "Empty message can't be sent to an agent".to_string()
            )
        );
    }

    #[tokio::test]
    async fn spawn_agent_rejects_when_message_and_items_are_both_set() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            function_payload(json!({
                "message": "hello",
                "items": [{"type": "mention", "name": "drive", "path": "app://drive"}]
            })),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("message+items should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(
                "Provide either message or items, but not both".to_string()
            )
        );
    }

    #[tokio::test]
    async fn spawn_agent_uses_explorer_role_and_preserves_approval_policy() {
        #[derive(Debug, Deserialize)]
        struct SpawnAgentResult {
            agent_id: String,
            nickname: Option<String>,
        }

        let (mut session, mut turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let mut config = (*turn.config).clone();
        config
            .permissions
            .approval_policy
            .set(AskForApproval::OnRequest)
            .expect("approval policy should be set");
        turn.approval_policy
            .set(AskForApproval::OnRequest)
            .expect("approval policy should be set");
        turn.config = Arc::new(config);

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            function_payload(json!({
                "message": "inspect this repo",
                "agent_type": "explorer"
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("spawn_agent should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: SpawnAgentResult =
            serde_json::from_str(&content).expect("spawn_agent result should be json");
        let agent_id = agent_id(&result.agent_id).expect("agent_id should be valid");
        assert!(
            result
                .nickname
                .as_deref()
                .is_some_and(|nickname| !nickname.is_empty())
        );
        let snapshot = manager
            .get_thread(agent_id)
            .await
            .expect("spawned agent thread should exist")
            .config_snapshot()
            .await;
        assert_eq!(snapshot.approval_policy, AskForApproval::OnRequest);
    }

    #[tokio::test]
    async fn spawn_agent_errors_when_manager_dropped() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            function_payload(json!({"message": "hello"})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("spawn should fail without a manager");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("collab manager unavailable".to_string())
        );
    }

    #[tokio::test]
    async fn spawn_agent_reapplies_runtime_sandbox_after_role_config() {
        fn pick_allowed_sandbox_policy(
            constraint: &crate::config::Constrained<SandboxPolicy>,
            base: SandboxPolicy,
        ) -> SandboxPolicy {
            let candidates = [
                SandboxPolicy::DangerFullAccess,
                SandboxPolicy::new_workspace_write_policy(),
                SandboxPolicy::new_read_only_policy(),
            ];
            candidates
                .into_iter()
                .find(|candidate| *candidate != base && constraint.can_set(candidate).is_ok())
                .unwrap_or(base)
        }

        #[derive(Debug, Deserialize)]
        struct SpawnAgentResult {
            agent_id: String,
            nickname: Option<String>,
        }

        let (mut session, mut turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let expected_sandbox = pick_allowed_sandbox_policy(
            &turn.config.permissions.sandbox_policy,
            turn.config.permissions.sandbox_policy.get().clone(),
        );
        turn.approval_policy
            .set(AskForApproval::OnRequest)
            .expect("approval policy should be set");
        turn.sandbox_policy
            .set(expected_sandbox.clone())
            .expect("sandbox policy should be set");
        assert_ne!(
            expected_sandbox,
            turn.config.permissions.sandbox_policy.get().clone(),
            "test requires a runtime sandbox override that differs from base config"
        );

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            function_payload(json!({
                "message": "await this command",
                "agent_type": "explorer"
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("spawn_agent should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: SpawnAgentResult =
            serde_json::from_str(&content).expect("spawn_agent result should be json");
        let agent_id = agent_id(&result.agent_id).expect("agent_id should be valid");
        assert!(
            result
                .nickname
                .as_deref()
                .is_some_and(|nickname| !nickname.is_empty())
        );

        let snapshot = manager
            .get_thread(agent_id)
            .await
            .expect("spawned agent thread should exist")
            .config_snapshot()
            .await;
        assert_eq!(snapshot.sandbox_policy, expected_sandbox);
        assert_eq!(snapshot.approval_policy, AskForApproval::OnRequest);
    }

    #[tokio::test]
    async fn spawn_agent_rejects_when_depth_limit_exceeded() {
        let (mut session, mut turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();

        let max_depth = turn.config.agent_max_depth;
        turn.session_source = SessionSource::SubAgent(SubAgentSource::ThreadSpawn {
            parent_thread_id: session.conversation_id,
            depth: max_depth,
            agent_nickname: None,
            agent_role: None,
        });

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            function_payload(json!({"message": "hello"})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("spawn should fail when depth limit exceeded");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(
                "Agent depth limit reached. Solve the task yourself.".to_string()
            )
        );
    }

    #[tokio::test]
    async fn spawn_agent_allows_depth_up_to_configured_max_depth() {
        #[derive(Debug, Deserialize)]
        struct SpawnAgentResult {
            agent_id: String,
            nickname: Option<String>,
        }

        let (mut session, mut turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();

        let mut config = (*turn.config).clone();
        config.agent_max_depth = DEFAULT_AGENT_MAX_DEPTH + 1;
        turn.config = Arc::new(config);
        turn.session_source = SessionSource::SubAgent(SubAgentSource::ThreadSpawn {
            parent_thread_id: session.conversation_id,
            depth: DEFAULT_AGENT_MAX_DEPTH,
            agent_nickname: None,
            agent_role: None,
        });

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "spawn_agent",
            function_payload(json!({"message": "hello"})),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("spawn should succeed within configured depth");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: SpawnAgentResult =
            serde_json::from_str(&content).expect("spawn_agent result should be json");
        assert!(!result.agent_id.is_empty());
        assert!(
            result
                .nickname
                .as_deref()
                .is_some_and(|nickname| !nickname.is_empty())
        );
        assert_eq!(success, Some(true));
    }

    #[tokio::test]
    async fn send_input_rejects_empty_message() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "send_input",
            function_payload(json!({"id": ThreadId::new().to_string(), "message": ""})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("empty message should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(
                "Empty message can't be sent to an agent".to_string()
            )
        );
    }

    #[tokio::test]
    async fn send_input_rejects_when_message_and_items_are_both_set() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "send_input",
            function_payload(json!({
                "id": ThreadId::new().to_string(),
                "message": "hello",
                "items": [{"type": "mention", "name": "drive", "path": "app://drive"}]
            })),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("message+items should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(
                "Provide either message or items, but not both".to_string()
            )
        );
    }

    #[tokio::test]
    async fn send_input_rejects_invalid_id() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "send_input",
            function_payload(json!({"id": "not-a-uuid", "message": "hi"})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("invalid id should be rejected");
        };
        let FunctionCallError::RespondToModel(msg) = err else {
            panic!("expected respond-to-model error");
        };
        assert!(msg.starts_with("invalid agent id not-a-uuid:"));
    }

    #[tokio::test]
    async fn send_input_reports_missing_agent() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let agent_id = ThreadId::new();
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "send_input",
            function_payload(json!({"id": agent_id.to_string(), "message": "hi"})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("missing agent should be reported");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(format!("agent with id {agent_id} not found"))
        );
    }

    #[tokio::test]
    async fn send_input_interrupts_before_prompt() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager.start_thread(config).await.expect("start thread");
        let agent_id = thread.thread_id;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "send_input",
            function_payload(json!({
                "id": agent_id.to_string(),
                "message": "hi",
                "interrupt": true
            })),
        );
        MultiAgentHandler
            .handle(invocation)
            .await
            .expect("send_input should succeed");

        let ops = manager.captured_ops();
        let ops_for_agent: Vec<&Op> = ops
            .iter()
            .filter_map(|(id, op)| (*id == agent_id).then_some(op))
            .collect();
        assert_eq!(ops_for_agent.len(), 2);
        assert!(matches!(ops_for_agent[0], Op::Interrupt));
        assert!(matches!(ops_for_agent[1], Op::UserInput { .. }));

        let _ = thread
            .thread
            .submit(Op::Shutdown {})
            .await
            .expect("shutdown should submit");
    }

    #[tokio::test]
    async fn send_input_accepts_structured_items() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager.start_thread(config).await.expect("start thread");
        let agent_id = thread.thread_id;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "send_input",
            function_payload(json!({
                "id": agent_id.to_string(),
                "items": [
                    {"type": "mention", "name": "drive", "path": "app://google_drive"},
                    {"type": "text", "text": "read the folder"}
                ]
            })),
        );
        MultiAgentHandler
            .handle(invocation)
            .await
            .expect("send_input should succeed");

        let expected = Op::UserInput {
            items: vec![
                UserInput::Mention {
                    name: "drive".to_string(),
                    path: "app://google_drive".to_string(),
                },
                UserInput::Text {
                    text: "read the folder".to_string(),
                    text_elements: Vec::new(),
                },
            ],
            final_output_json_schema: None,
        };
        let captured = manager
            .captured_ops()
            .into_iter()
            .find(|(id, op)| *id == agent_id && *op == expected);
        assert_eq!(captured, Some((agent_id, expected)));

        let _ = thread
            .thread
            .submit(Op::Shutdown {})
            .await
            .expect("shutdown should submit");
    }

    #[tokio::test]
    async fn resume_agent_rejects_invalid_id() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "resume_agent",
            function_payload(json!({"id": "not-a-uuid"})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("invalid id should be rejected");
        };
        let FunctionCallError::RespondToModel(msg) = err else {
            panic!("expected respond-to-model error");
        };
        assert!(msg.starts_with("invalid agent id not-a-uuid:"));
    }

    #[tokio::test]
    async fn resume_agent_reports_missing_agent() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let agent_id = ThreadId::new();
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "resume_agent",
            function_payload(json!({"id": agent_id.to_string()})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("missing agent should be reported");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(format!("agent with id {agent_id} not found"))
        );
    }

    #[tokio::test]
    async fn resume_agent_noops_for_active_agent() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager.start_thread(config).await.expect("start thread");
        let agent_id = thread.thread_id;
        let status_before = manager.agent_control().get_status(agent_id).await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "resume_agent",
            function_payload(json!({"id": agent_id.to_string()})),
        );

        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("resume_agent should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: resume_agent::ResumeAgentResult =
            serde_json::from_str(&content).expect("resume_agent result should be json");
        assert_eq!(result.status, status_before);
        assert_eq!(success, Some(true));

        let thread_ids = manager.list_thread_ids().await;
        assert_eq!(thread_ids, vec![agent_id]);

        let _ = thread
            .thread
            .submit(Op::Shutdown {})
            .await
            .expect("shutdown should submit");
    }

    #[tokio::test]
    async fn resume_agent_restores_closed_agent_and_accepts_send_input() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager
            .resume_thread_with_history(
                config,
                InitialHistory::Forked(vec![RolloutItem::ResponseItem(ResponseItem::Message {
                    id: None,
                    role: "user".to_string(),
                    content: vec![ContentItem::InputText {
                        text: "materialized".to_string(),
                    }],
                    end_turn: None,
                    phase: None,
                })]),
                AuthManager::from_auth_for_testing(CodexAuth::from_api_key("dummy")),
                false,
            )
            .await
            .expect("start thread");
        let agent_id = thread.thread_id;
        let _ = manager
            .agent_control()
            .shutdown_agent(agent_id)
            .await
            .expect("shutdown agent");
        assert_eq!(
            manager.agent_control().get_status(agent_id).await,
            AgentStatus::NotFound
        );
        let session = Arc::new(session);
        let turn = Arc::new(turn);

        let resume_invocation = invocation(
            session.clone(),
            turn.clone(),
            "resume_agent",
            function_payload(json!({"id": agent_id.to_string()})),
        );
        let output = MultiAgentHandler
            .handle(resume_invocation)
            .await
            .expect("resume_agent should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: resume_agent::ResumeAgentResult =
            serde_json::from_str(&content).expect("resume_agent result should be json");
        assert_ne!(result.status, AgentStatus::NotFound);
        assert_eq!(success, Some(true));

        let send_invocation = invocation(
            session,
            turn,
            "send_input",
            function_payload(json!({"id": agent_id.to_string(), "message": "hello"})),
        );
        let output = MultiAgentHandler
            .handle(send_invocation)
            .await
            .expect("send_input should succeed after resume");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: serde_json::Value =
            serde_json::from_str(&content).expect("send_input result should be json");
        let submission_id = result
            .get("submission_id")
            .and_then(|value| value.as_str())
            .unwrap_or_default();
        assert!(!submission_id.is_empty());
        assert_eq!(success, Some(true));

        let _ = manager
            .agent_control()
            .shutdown_agent(agent_id)
            .await
            .expect("shutdown resumed agent");
    }

    #[tokio::test]
    async fn resume_agent_rejects_when_depth_limit_exceeded() {
        let (mut session, mut turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();

        let max_depth = turn.config.agent_max_depth;
        turn.session_source = SessionSource::SubAgent(SubAgentSource::ThreadSpawn {
            parent_thread_id: session.conversation_id,
            depth: max_depth,
            agent_nickname: None,
            agent_role: None,
        });

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "resume_agent",
            function_payload(json!({"id": ThreadId::new().to_string()})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("resume should fail when depth limit exceeded");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel(
                "Agent depth limit reached. Solve the task yourself.".to_string()
            )
        );
    }

    #[tokio::test]
    async fn wait_rejects_non_positive_timeout() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait",
            function_payload(json!({
                "ids": [ThreadId::new().to_string()],
                "timeout_ms": 0
            })),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("non-positive timeout should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("timeout_ms must be greater than zero".to_string())
        );
    }

    #[tokio::test]
    async fn wait_rejects_invalid_id() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait",
            function_payload(json!({"ids": ["invalid"]})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("invalid id should be rejected");
        };
        let FunctionCallError::RespondToModel(msg) = err else {
            panic!("expected respond-to-model error");
        };
        assert!(msg.starts_with("invalid agent id invalid:"));
    }

    #[tokio::test]
    async fn wait_rejects_empty_ids() {
        let (session, turn) = make_session_and_context().await;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait",
            function_payload(json!({"ids": []})),
        );
        let Err(err) = MultiAgentHandler.handle(invocation).await else {
            panic!("empty ids should be rejected");
        };
        assert_eq!(
            err,
            FunctionCallError::RespondToModel("ids must be non-empty".to_string())
        );
    }

    #[tokio::test]
    async fn wait_returns_not_found_for_missing_agents() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let id_a = ThreadId::new();
        let id_b = ThreadId::new();
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait",
            function_payload(json!({
                "ids": [id_a.to_string(), id_b.to_string()],
                "timeout_ms": 1000
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("wait should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: wait::WaitResult =
            serde_json::from_str(&content).expect("wait result should be json");
        assert_eq!(
            result,
            wait::WaitResult {
                status: HashMap::from([
                    (id_a, AgentStatus::NotFound),
                    (id_b, AgentStatus::NotFound),
                ]),
                timed_out: false
            }
        );
        assert_eq!(success, None);
    }

    #[tokio::test]
    async fn wait_times_out_when_status_is_not_final() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager.start_thread(config).await.expect("start thread");
        let agent_id = thread.thread_id;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait",
            function_payload(json!({
                "ids": [agent_id.to_string()],
                "timeout_ms": MIN_WAIT_TIMEOUT_MS
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("wait should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: wait::WaitResult =
            serde_json::from_str(&content).expect("wait result should be json");
        assert_eq!(
            result,
            wait::WaitResult {
                status: HashMap::new(),
                timed_out: true
            }
        );
        assert_eq!(success, None);

        let _ = thread
            .thread
            .submit(Op::Shutdown {})
            .await
            .expect("shutdown should submit");
    }

    #[tokio::test]
    async fn wait_clamps_short_timeouts_to_minimum() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager.start_thread(config).await.expect("start thread");
        let agent_id = thread.thread_id;
        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait",
            function_payload(json!({
                "ids": [agent_id.to_string()],
                "timeout_ms": 10
            })),
        );

        let early = timeout(
            Duration::from_millis(50),
            MultiAgentHandler.handle(invocation),
        )
        .await;
        assert!(
            early.is_err(),
            "wait should not return before the minimum timeout clamp"
        );

        let _ = thread
            .thread
            .submit(Op::Shutdown {})
            .await
            .expect("shutdown should submit");
    }

    #[tokio::test]
    async fn wait_returns_final_status_without_timeout() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager.start_thread(config).await.expect("start thread");
        let agent_id = thread.thread_id;
        let mut status_rx = manager
            .agent_control()
            .subscribe_status(agent_id)
            .await
            .expect("subscribe should succeed");

        let _ = thread
            .thread
            .submit(Op::Shutdown {})
            .await
            .expect("shutdown should submit");
        let _ = timeout(Duration::from_secs(1), status_rx.changed())
            .await
            .expect("shutdown status should arrive");

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "wait",
            function_payload(json!({
                "ids": [agent_id.to_string()],
                "timeout_ms": 1000
            })),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("wait should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: wait::WaitResult =
            serde_json::from_str(&content).expect("wait result should be json");
        assert_eq!(
            result,
            wait::WaitResult {
                status: HashMap::from([(agent_id, AgentStatus::Shutdown)]),
                timed_out: false
            }
        );
        assert_eq!(success, None);
    }

    #[tokio::test]
    async fn close_agent_submits_shutdown_and_returns_status() {
        let (mut session, turn) = make_session_and_context().await;
        let manager = thread_manager();
        session.services.agent_control = manager.agent_control();
        let config = turn.config.as_ref().clone();
        let thread = manager.start_thread(config).await.expect("start thread");
        let agent_id = thread.thread_id;
        let status_before = manager.agent_control().get_status(agent_id).await;

        let invocation = invocation(
            Arc::new(session),
            Arc::new(turn),
            "close_agent",
            function_payload(json!({"id": agent_id.to_string()})),
        );
        let output = MultiAgentHandler
            .handle(invocation)
            .await
            .expect("close_agent should succeed");
        let ToolOutput::Function {
            body: FunctionCallOutputBody::Text(content),
            success,
            ..
        } = output
        else {
            panic!("expected function output");
        };
        let result: close_agent::CloseAgentResult =
            serde_json::from_str(&content).expect("close_agent result should be json");
        assert_eq!(result.status, status_before);
        assert_eq!(success, Some(true));

        let ops = manager.captured_ops();
        let submitted_shutdown = ops
            .iter()
            .any(|(id, op)| *id == agent_id && matches!(op, Op::Shutdown));
        assert_eq!(submitted_shutdown, true);

        let status_after = manager.agent_control().get_status(agent_id).await;
        assert_eq!(status_after, AgentStatus::NotFound);
    }

    #[tokio::test]
    async fn build_agent_spawn_config_uses_turn_context_values() {
        fn pick_allowed_sandbox_policy(
            constraint: &crate::config::Constrained<SandboxPolicy>,
            base: SandboxPolicy,
        ) -> SandboxPolicy {
            let candidates = [
                SandboxPolicy::new_read_only_policy(),
                SandboxPolicy::new_workspace_write_policy(),
                SandboxPolicy::DangerFullAccess,
            ];
            candidates
                .into_iter()
                .find(|candidate| *candidate != base && constraint.can_set(candidate).is_ok())
                .unwrap_or(base)
        }

        let (_session, mut turn) = make_session_and_context().await;
        let base_instructions = BaseInstructions {
            text: "base".to_string(),
        };
        turn.developer_instructions = Some("dev".to_string());
        turn.compact_prompt = Some("compact".to_string());
        turn.shell_environment_policy = ShellEnvironmentPolicy {
            use_profile: true,
            ..ShellEnvironmentPolicy::default()
        };
        let temp_dir = tempfile::tempdir().expect("temp dir");
        turn.cwd = temp_dir.path().to_path_buf();
        turn.codex_linux_sandbox_exe = Some(PathBuf::from("/bin/echo"));
        let sandbox_policy = pick_allowed_sandbox_policy(
            &turn.config.permissions.sandbox_policy,
            turn.config.permissions.sandbox_policy.get().clone(),
        );
        turn.sandbox_policy
            .set(sandbox_policy)
            .expect("sandbox policy set");
        turn.approval_policy
            .set(AskForApproval::OnRequest)
            .expect("approval policy set");

        let config = build_agent_spawn_config(&base_instructions, &turn).expect("spawn config");
        let mut expected = (*turn.config).clone();
        expected.base_instructions = Some(base_instructions.text);
        expected.model = Some(turn.model_info.slug.clone());
        expected.model_provider = turn.provider.clone();
        expected.model_reasoning_effort = turn.reasoning_effort;
        expected.model_reasoning_summary = Some(turn.reasoning_summary);
        expected.developer_instructions = turn.developer_instructions.clone();
        expected.compact_prompt = turn.compact_prompt.clone();
        expected.permissions.shell_environment_policy = turn.shell_environment_policy.clone();
        expected.codex_linux_sandbox_exe = turn.codex_linux_sandbox_exe.clone();
        expected.cwd = turn.cwd.clone();
        expected
            .permissions
            .approval_policy
            .set(AskForApproval::OnRequest)
            .expect("approval policy set");
        expected
            .permissions
            .sandbox_policy
            .set(turn.sandbox_policy.get().clone())
            .expect("sandbox policy set");
        assert_eq!(config, expected);
    }

    #[tokio::test]
    async fn build_agent_spawn_config_preserves_base_user_instructions() {
        let (_session, mut turn) = make_session_and_context().await;
        let mut base_config = (*turn.config).clone();
        base_config.user_instructions = Some("base-user".to_string());
        turn.user_instructions = Some("resolved-user".to_string());
        turn.config = Arc::new(base_config.clone());
        let base_instructions = BaseInstructions {
            text: "base".to_string(),
        };

        let config = build_agent_spawn_config(&base_instructions, &turn).expect("spawn config");

        assert_eq!(config.user_instructions, base_config.user_instructions);
    }

    #[tokio::test]
    async fn build_agent_resume_config_clears_base_instructions() {
        let (_session, mut turn) = make_session_and_context().await;
        let mut base_config = (*turn.config).clone();
        base_config.base_instructions = Some("caller-base".to_string());
        turn.config = Arc::new(base_config);
        turn.approval_policy
            .set(AskForApproval::OnRequest)
            .expect("approval policy set");

        let config = build_agent_resume_config(&turn, 0).expect("resume config");

        let mut expected = (*turn.config).clone();
        expected.base_instructions = None;
        expected.model = Some(turn.model_info.slug.clone());
        expected.model_provider = turn.provider.clone();
        expected.model_reasoning_effort = turn.reasoning_effort;
        expected.model_reasoning_summary = Some(turn.reasoning_summary);
        expected.developer_instructions = turn.developer_instructions.clone();
        expected.compact_prompt = turn.compact_prompt.clone();
        expected.permissions.shell_environment_policy = turn.shell_environment_policy.clone();
        expected.codex_linux_sandbox_exe = turn.codex_linux_sandbox_exe.clone();
        expected.cwd = turn.cwd.clone();
        expected
            .permissions
            .approval_policy
            .set(AskForApproval::OnRequest)
            .expect("approval policy set");
        expected
            .permissions
            .sandbox_policy
            .set(turn.sandbox_policy.get().clone())
            .expect("sandbox policy set");
        assert_eq!(config, expected);
    }
}
