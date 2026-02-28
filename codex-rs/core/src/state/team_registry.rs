use codex_protocol::ThreadId;
use std::collections::HashMap;

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

#[derive(Default, Debug)]
pub(crate) struct TeamRegistry {
    teams: HashMap<String, TeamState>,
    next_team_id: u64,
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
    pub(crate) fn create_team(
        &mut self,
        team_name: Option<&str>,
        lead: TeamMember,
        workers: Vec<TeamMember>,
        auto_cleanup: bool,
    ) -> Result<TeamState, String> {
        let lead_name = sanitize_non_empty(&lead.name, "lead_name")?;
        let normalized_lead_name = normalize_name(&lead_name);
        let lead_thread_id = lead.thread_id;

        let mut members = HashMap::new();
        members.insert(
            normalized_lead_name.clone(),
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
        Ok(state)
    }

    pub(crate) fn team(&self, team_id: &str) -> Option<TeamState> {
        self.teams.get(team_id).cloned()
    }

    pub(crate) fn remove_team(&mut self, team_id: &str) -> Option<TeamState> {
        self.teams.remove(team_id)
    }

    pub(crate) fn teams(&self) -> Vec<TeamState> {
        self.teams.values().cloned().collect()
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use codex_protocol::ThreadId;
    use pretty_assertions::assert_eq;

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
}
