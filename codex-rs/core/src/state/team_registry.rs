use codex_protocol::ThreadId;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use uuid::Uuid;

const TEAM_PERSIST_DIR: &str = "teams";
const TEAM_CONFIG_FILENAME: &str = "config.json";
const TEAM_PERSIST_SCHEMA_VERSION: u32 = 1;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TeamMember {
    pub(crate) name: String,
    pub(crate) thread_id: ThreadId,
    pub(crate) nickname: Option<String>,
    pub(crate) role: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TeamState {
    pub(crate) team_id: String,
    pub(crate) team_name: String,
    pub(crate) lead_name: String,
    pub(crate) lead_thread_id: ThreadId,
    pub(crate) auto_cleanup: bool,
    members: HashMap<String, TeamMember>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TeamMembershipInfo {
    pub(crate) team_id: String,
    pub(crate) member_name: String,
    pub(crate) lead_thread_id: ThreadId,
}

#[derive(Default, Debug)]
pub(crate) struct TeamRegistry {
    teams: HashMap<String, TeamState>,
    team_dirs: HashMap<String, PathBuf>,
    next_team_id: u64,
    persist_dir: Option<PathBuf>,
    loaded_from_disk: bool,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
struct PersistedTeamMember {
    name: String,
    thread_id: String,
    nickname: Option<String>,
    role: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
struct PersistedTeamConfig {
    schema_version: u32,
    team_id: String,
    team_name: String,
    lead_name: String,
    lead_thread_id: String,
    auto_cleanup: bool,
    members: Vec<PersistedTeamMember>,
}

impl TeamState {
    pub(crate) fn member(&self, name: &str) -> Option<&TeamMember> {
        self.members.get(&normalize_name(name))
    }

    pub(crate) fn member_by_thread_id(&self, thread_id: ThreadId) -> Option<&TeamMember> {
        self.members
            .values()
            .find(|member| member.thread_id == thread_id)
    }

    pub(crate) fn members(&self) -> impl Iterator<Item = &TeamMember> {
        self.members.values()
    }

    pub(crate) fn member_count(&self) -> usize {
        self.members.len()
    }
}

impl TeamRegistry {
    pub(crate) fn configure_persistence(&mut self, codex_home: &Path) -> Result<(), String> {
        let persist_dir = codex_home.join(TEAM_PERSIST_DIR);
        if self.persist_dir.as_ref() == Some(&persist_dir) && self.loaded_from_disk {
            return Ok(());
        }
        self.persist_dir = Some(persist_dir);
        self.loaded_from_disk = false;
        self.load_from_disk_if_needed()
    }

    pub(crate) fn create_team(
        &mut self,
        team_name: Option<&str>,
        lead: TeamMember,
        workers: Vec<TeamMember>,
        auto_cleanup: bool,
    ) -> Result<TeamState, String> {
        self.load_from_disk_if_needed()?;

        let lead_name = sanitize_non_empty(&lead.name, "lead_name")?;
        let normalized_lead_name = normalize_name(&lead_name);
        let lead_thread_id = lead.thread_id;

        let mut members = HashMap::new();
        members.insert(
            normalized_lead_name,
            TeamMember {
                name: lead_name.clone(),
                ..lead
            },
        );

        for worker in workers {
            let worker_name = sanitize_non_empty(&worker.name, "member_name")?;
            let key = normalize_name(&worker_name);
            if members.contains_key(&key) {
                return Err(format!("duplicate team member name: {worker_name}"));
            }
            members.insert(
                key,
                TeamMember {
                    name: worker_name,
                    ..worker
                },
            );
        }

        self.next_team_id = self.next_team_id.saturating_add(1);
        let team_id = format!("team-{}", self.next_team_id);
        let normalized_team_name = team_name
            .map(str::trim)
            .filter(|name| !name.is_empty())
            .map(ToOwned::to_owned)
            .unwrap_or_else(|| format!("team-{}", self.next_team_id));
        let state = TeamState {
            team_id: team_id.clone(),
            team_name: normalized_team_name,
            lead_name,
            lead_thread_id,
            auto_cleanup,
            members,
        };
        self.teams.insert(team_id, state.clone());
        self.persist_team(&state)?;
        Ok(state)
    }

    pub(crate) fn team(&self, team_id: &str) -> Option<TeamState> {
        self.teams.get(team_id).cloned()
    }

    pub(crate) fn remove_team(&mut self, team_id: &str) -> Option<TeamState> {
        let removed = self.teams.remove(team_id);
        if removed.is_some()
            && let Err(err) = self.remove_persisted_team(team_id)
        {
            tracing::warn!(team_id, %err, "failed to remove persisted team");
        }
        removed
    }

    pub(crate) fn teams(&self) -> Vec<TeamState> {
        self.teams.values().cloned().collect()
    }

    /// Cheap lookup: is this thread_id a member of any team?
    /// Returns (team_id, member_name, lead_thread_id) if found.
    /// Skips members whose thread_id matches the lead (the lead doesn't need self-notification).
    pub(crate) fn team_membership(&self, thread_id: ThreadId) -> Option<TeamMembershipInfo> {
        self.teams.values().find_map(|team| {
            if thread_id == team.lead_thread_id {
                return None;
            }
            team.member_by_thread_id(thread_id)
                .map(|member| TeamMembershipInfo {
                    team_id: team.team_id.clone(),
                    member_name: member.name.clone(),
                    lead_thread_id: team.lead_thread_id,
                })
        })
    }

    fn load_from_disk_if_needed(&mut self) -> Result<(), String> {
        if self.loaded_from_disk {
            return Ok(());
        }
        let Some(persist_dir) = self.persist_dir.clone() else {
            self.loaded_from_disk = true;
            return Ok(());
        };
        self.load_from_disk(&persist_dir)
    }

    fn load_from_disk(&mut self, persist_dir: &Path) -> Result<(), String> {
        fs::create_dir_all(persist_dir).map_err(|err| {
            format!(
                "failed to create team persistence directory {}: {err}",
                persist_dir.display()
            )
        })?;

        for team in self.teams.values() {
            if let Some(id) = parse_team_numeric_id(&team.team_id) {
                self.next_team_id = self.next_team_id.max(id);
            }
        }

        let entries = fs::read_dir(persist_dir).map_err(|err| {
            format!(
                "failed to read team persistence directory {}: {err}",
                persist_dir.display()
            )
        })?;
        for entry in entries {
            let Ok(entry) = entry else {
                continue;
            };
            let team_dir = entry.path();
            if !team_dir.is_dir() {
                continue;
            }
            let config_path = team_dir.join(TEAM_CONFIG_FILENAME);
            if !config_path.exists() {
                continue;
            }
            let serialized = match fs::read_to_string(&config_path) {
                Ok(content) => content,
                Err(err) => {
                    tracing::warn!(path = %config_path.display(), %err, "failed to read team config");
                    continue;
                }
            };
            let persisted = match serde_json::from_str::<PersistedTeamConfig>(&serialized) {
                Ok(config) => config,
                Err(err) => {
                    tracing::warn!(path = %config_path.display(), %err, "failed to parse team config");
                    continue;
                }
            };
            let team = match team_from_persisted(persisted) {
                Ok(team) => team,
                Err(err) => {
                    tracing::warn!(path = %config_path.display(), %err, "failed to decode team config");
                    continue;
                }
            };
            if let Some(id) = parse_team_numeric_id(&team.team_id) {
                self.next_team_id = self.next_team_id.max(id);
            }
            if self.teams.contains_key(&team.team_id) {
                continue;
            }
            self.team_dirs.insert(team.team_id.clone(), team_dir);
            self.teams.insert(team.team_id.clone(), team);
        }
        self.loaded_from_disk = true;
        Ok(())
    }

    fn persist_team(&mut self, team: &TeamState) -> Result<(), String> {
        let Some(persist_root) = self.persist_dir.clone() else {
            return Ok(());
        };
        fs::create_dir_all(&persist_root).map_err(|err| {
            format!(
                "failed to create team persistence directory {}: {err}",
                persist_root.display()
            )
        })?;

        let team_dir = if let Some(existing) = self.team_dirs.get(&team.team_id) {
            existing.clone()
        } else {
            let dir = persist_root.join(format!("{}-{}", slugify(&team.team_name), team.team_id));
            self.team_dirs.insert(team.team_id.clone(), dir.clone());
            dir
        };
        fs::create_dir_all(&team_dir).map_err(|err| {
            format!(
                "failed to create team directory {}: {err}",
                team_dir.display()
            )
        })?;

        let persisted = persisted_from_team(team);
        let serialized = serde_json::to_vec_pretty(&persisted)
            .map_err(|err| format!("failed to serialize team config {}: {err}", team.team_id))?;
        let config_path = team_dir.join(TEAM_CONFIG_FILENAME);
        write_json_atomic(&config_path, &serialized)
    }

    fn remove_persisted_team(&mut self, team_id: &str) -> Result<(), String> {
        let Some(team_dir) = self.team_dirs.remove(team_id) else {
            return Ok(());
        };
        if !team_dir.exists() {
            return Ok(());
        }
        fs::remove_dir_all(&team_dir).map_err(|err| {
            format!(
                "failed to remove team directory {}: {err}",
                team_dir.display()
            )
        })
    }
}

fn team_from_persisted(persisted: PersistedTeamConfig) -> Result<TeamState, String> {
    if persisted.schema_version > TEAM_PERSIST_SCHEMA_VERSION {
        return Err(format!(
            "unsupported team config schema_version {}",
            persisted.schema_version
        ));
    }
    let lead_name = sanitize_non_empty(&persisted.lead_name, "lead_name")?;
    let lead_thread_id = ThreadId::from_string(&persisted.lead_thread_id)
        .map_err(|err| format!("invalid lead_thread_id: {err}"))?;
    let team_id = sanitize_non_empty(&persisted.team_id, "team_id")?;
    let team_name = sanitize_non_empty(&persisted.team_name, "team_name")?;

    let mut members = HashMap::new();
    for member in persisted.members {
        let member_name = sanitize_non_empty(&member.name, "member_name")?;
        let key = normalize_name(&member_name);
        if members.contains_key(&key) {
            return Err(format!("duplicate team member name: {member_name}"));
        }
        let thread_id = ThreadId::from_string(&member.thread_id)
            .map_err(|err| format!("invalid member thread_id for {member_name}: {err}"))?;
        members.insert(
            key,
            TeamMember {
                name: member_name,
                thread_id,
                nickname: member.nickname,
                role: member.role,
            },
        );
    }

    let normalized_lead_name = normalize_name(&lead_name);
    members
        .entry(normalized_lead_name)
        .or_insert_with(|| TeamMember {
            name: lead_name.clone(),
            thread_id: lead_thread_id,
            nickname: None,
            role: Some("team-lead".to_string()),
        });

    Ok(TeamState {
        team_id,
        team_name,
        lead_name,
        lead_thread_id,
        auto_cleanup: persisted.auto_cleanup,
        members,
    })
}

fn persisted_from_team(team: &TeamState) -> PersistedTeamConfig {
    let mut members = team
        .members()
        .map(|member| PersistedTeamMember {
            name: member.name.clone(),
            thread_id: member.thread_id.to_string(),
            nickname: member.nickname.clone(),
            role: member.role.clone(),
        })
        .collect::<Vec<_>>();
    members.sort_by(|left, right| left.name.cmp(&right.name));

    PersistedTeamConfig {
        schema_version: TEAM_PERSIST_SCHEMA_VERSION,
        team_id: team.team_id.clone(),
        team_name: team.team_name.clone(),
        lead_name: team.lead_name.clone(),
        lead_thread_id: team.lead_thread_id.to_string(),
        auto_cleanup: team.auto_cleanup,
        members,
    }
}

fn parse_team_numeric_id(team_id: &str) -> Option<u64> {
    team_id.strip_prefix("team-")?.parse::<u64>().ok()
}

fn normalize_name(name: &str) -> String {
    name.trim().to_ascii_lowercase()
}

fn sanitize_non_empty(value: &str, field: &str) -> Result<String, String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(format!("{field} cannot be empty"));
    }
    Ok(trimmed.to_string())
}

fn slugify(name: &str) -> String {
    let mut slug = String::new();
    let mut pending_dash = false;
    for ch in name.trim().chars() {
        if ch.is_ascii_alphanumeric() {
            if pending_dash && !slug.is_empty() {
                slug.push('-');
            }
            slug.push(ch.to_ascii_lowercase());
            pending_dash = false;
        } else {
            pending_dash = true;
        }
    }
    if slug.is_empty() {
        return "team".to_string();
    }
    slug
}

fn write_json_atomic(path: &Path, payload: &[u8]) -> Result<(), String> {
    let tmp_path = path.with_extension(format!("tmp-{}", Uuid::new_v4()));
    fs::write(&tmp_path, payload)
        .map_err(|err| format!("failed to write temp file {}: {err}", tmp_path.display()))?;
    if let Err(err) = fs::rename(&tmp_path, path) {
        let _ = fs::remove_file(&tmp_path);
        return Err(format!(
            "failed to rename {} to {}: {err}",
            tmp_path.display(),
            path.display()
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use codex_protocol::ThreadId;
    use pretty_assertions::assert_eq;
    use tempfile::tempdir;

    fn thread(id: &str) -> ThreadId {
        ThreadId::from_string(id).expect("valid thread id")
    }

    #[test]
    fn create_team_rejects_duplicate_names_case_insensitively() {
        let mut registry = TeamRegistry::default();
        let result = registry.create_team(
            Some("alpha"),
            TeamMember {
                name: "Lead".to_string(),
                thread_id: thread("019ca37e-d7fb-75d1-bf05-afc056f49305"),
                nickname: None,
                role: Some("team-lead".to_string()),
            },
            vec![TeamMember {
                name: "lead".to_string(),
                thread_id: thread("019ca381-3a17-77b0-aa54-1161c2033360"),
                nickname: None,
                role: Some("runtime-agent".to_string()),
            }],
            false,
        );
        assert_eq!(result, Err("duplicate team member name: lead".to_string()));
    }

    #[test]
    fn create_team_and_lookup_by_name_and_thread() {
        let lead_id = thread("019ca37e-d7fb-75d1-bf05-afc056f49305");
        let worker_id = thread("019ca381-3a17-77b0-aa54-1161c2033360");

        let mut registry = TeamRegistry::default();
        let team = registry
            .create_team(
                Some("alpha"),
                TeamMember {
                    name: "orchestrator".to_string(),
                    thread_id: lead_id,
                    nickname: None,
                    role: Some("team-lead".to_string()),
                },
                vec![TeamMember {
                    name: "Feynman".to_string(),
                    thread_id: worker_id,
                    nickname: Some("Feynman".to_string()),
                    role: Some("runtime-agent".to_string()),
                }],
                true,
            )
            .expect("team should be created");

        assert_eq!(team.lead_thread_id, lead_id);
        assert_eq!(team.auto_cleanup, true);
        assert_eq!(team.member_count(), 2);
        assert_eq!(
            team.member("feynman").map(|member| member.thread_id),
            Some(worker_id)
        );
        assert_eq!(
            team.member_by_thread_id(lead_id)
                .map(|member| member.name.as_str()),
            Some("orchestrator")
        );
    }

    #[test]
    fn teams_lists_created_teams() {
        let mut registry = TeamRegistry::default();
        let _ = registry.create_team(
            Some("alpha"),
            TeamMember {
                name: "lead".to_string(),
                thread_id: thread("019ca37e-d7fb-75d1-bf05-afc056f49305"),
                nickname: None,
                role: Some("team-lead".to_string()),
            },
            vec![],
            false,
        );
        let _ = registry.create_team(
            Some("beta"),
            TeamMember {
                name: "lead2".to_string(),
                thread_id: thread("019ca381-3a17-77b0-aa54-1161c2033360"),
                nickname: None,
                role: Some("team-lead".to_string()),
            },
            vec![],
            true,
        );
        let mut teams = registry.teams();
        teams.sort_by(|left, right| left.team_name.cmp(&right.team_name));
        assert_eq!(teams.len(), 2);
        assert_eq!(teams[0].team_name, "alpha");
        assert_eq!(teams[1].team_name, "beta");
        assert_eq!(teams[0].auto_cleanup, false);
        assert_eq!(teams[1].auto_cleanup, true);
    }

    #[test]
    fn configure_persistence_restores_teams_from_disk() {
        let home = tempdir().expect("tempdir");
        let lead_id = thread("019ca37e-d7fb-75d1-bf05-afc056f49305");
        let worker_id = thread("019ca381-3a17-77b0-aa54-1161c2033360");

        let team_id = {
            let mut registry = TeamRegistry::default();
            registry
                .configure_persistence(home.path())
                .expect("configure persistence");
            registry
                .create_team(
                    Some("alpha"),
                    TeamMember {
                        name: "lead".to_string(),
                        thread_id: lead_id,
                        nickname: None,
                        role: Some("team-lead".to_string()),
                    },
                    vec![TeamMember {
                        name: "worker".to_string(),
                        thread_id: worker_id,
                        nickname: Some("Worker".to_string()),
                        role: Some("runtime-agent".to_string()),
                    }],
                    false,
                )
                .expect("team should be created")
                .team_id
        };

        let mut restored = TeamRegistry::default();
        restored
            .configure_persistence(home.path())
            .expect("configure persistence");
        let restored_team = restored.team(&team_id).expect("team should be restored");
        assert_eq!(restored_team.team_name, "alpha");
        assert_eq!(
            restored_team
                .member_by_thread_id(worker_id)
                .map(|member| member.name.as_str()),
            Some("worker")
        );

        let next = restored
            .create_team(
                Some("beta"),
                TeamMember {
                    name: "lead-2".to_string(),
                    thread_id: ThreadId::new(),
                    nickname: None,
                    role: Some("team-lead".to_string()),
                },
                vec![],
                false,
            )
            .expect("second team should be created");
        assert_eq!(next.team_id, "team-2");
    }

    #[test]
    fn remove_team_deletes_persisted_state() {
        let home = tempdir().expect("tempdir");

        let (team_id, team_dir) = {
            let mut registry = TeamRegistry::default();
            registry
                .configure_persistence(home.path())
                .expect("configure persistence");
            let team = registry
                .create_team(
                    Some("alpha"),
                    TeamMember {
                        name: "lead".to_string(),
                        thread_id: ThreadId::new(),
                        nickname: None,
                        role: Some("team-lead".to_string()),
                    },
                    vec![],
                    false,
                )
                .expect("team should be created");
            let team_dir = home.path().join(TEAM_PERSIST_DIR).join(format!(
                "{}-{}",
                slugify(&team.team_name),
                team.team_id
            ));
            (team.team_id, team_dir)
        };

        let mut registry = TeamRegistry::default();
        registry
            .configure_persistence(home.path())
            .expect("configure persistence");
        assert!(registry.remove_team(&team_id).is_some());
        assert!(!team_dir.exists(), "team directory should be removed");
    }

    #[test]
    fn team_membership_finds_non_lead_members() {
        let lead_id = thread("019ca37e-d7fb-75d1-bf05-afc056f49305");
        let worker_id = thread("019ca381-3a17-77b0-aa54-1161c2033360");
        let mut registry = TeamRegistry::default();
        let team = registry
            .create_team(
                Some("alpha"),
                TeamMember {
                    name: "lead".to_string(),
                    thread_id: lead_id,
                    nickname: None,
                    role: Some("team-lead".to_string()),
                },
                vec![TeamMember {
                    name: "worker".to_string(),
                    thread_id: worker_id,
                    nickname: None,
                    role: Some("runtime-agent".to_string()),
                }],
                false,
            )
            .expect("team should be created");

        assert_eq!(
            registry.team_membership(worker_id),
            Some(TeamMembershipInfo {
                team_id: team.team_id,
                member_name: "worker".to_string(),
                lead_thread_id: lead_id,
            })
        );
        assert_eq!(registry.team_membership(lead_id), None);
    }
}
