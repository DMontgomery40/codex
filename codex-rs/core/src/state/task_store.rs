use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fs;
use std::fs::OpenOptions;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;
use uuid::Uuid;

const TASK_ROOT_DIR: &str = "tasks";
const LOCK_FILENAME: &str = ".lock";
const LOCK_RETRIES: usize = 10;
const LOCK_RETRY_SLEEP: Duration = Duration::from_millis(50);

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum TaskStatus {
    Pending,
    InProgress,
    Blocked,
    Completed,
    Cancelled,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
pub(crate) struct Task {
    pub(crate) id: String,
    pub(crate) subject: String,
    pub(crate) description: Option<String>,
    pub(crate) status: TaskStatus,
    pub(crate) owner: Option<String>,
    pub(crate) active_form: Option<String>,
    pub(crate) blocks: Vec<String>,
    pub(crate) blocked_by: Vec<String>,
    pub(crate) metadata: BTreeMap<String, Value>,
    pub(crate) created_at: i64,
    pub(crate) updated_at: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct NewTask {
    pub(crate) subject: String,
    pub(crate) description: Option<String>,
    pub(crate) owner: Option<String>,
    pub(crate) active_form: Option<String>,
    pub(crate) blocks: Vec<String>,
    pub(crate) blocked_by: Vec<String>,
    pub(crate) metadata: BTreeMap<String, Value>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct TaskUpdate {
    pub(crate) status: Option<TaskStatus>,
    pub(crate) owner: Option<Option<String>>,
    pub(crate) subject: Option<String>,
    pub(crate) description: Option<Option<String>>,
    pub(crate) active_form: Option<Option<String>>,
    pub(crate) blocks: Option<Vec<String>>,
    pub(crate) blocked_by: Option<Vec<String>>,
    pub(crate) metadata: Option<BTreeMap<String, Value>>,
}

#[derive(Clone, Debug)]
pub(crate) struct TaskStore {
    team_dir: PathBuf,
}

impl TaskStore {
    pub(crate) fn new(codex_home: &Path, team_id: &str, team_name: &str) -> Self {
        Self {
            team_dir: codex_home.join(TASK_ROOT_DIR).join(format!(
                "{}-{}",
                slugify(team_name),
                slugify(team_id)
            )),
        }
    }

    pub(crate) fn create_task(&self, mut input: NewTask) -> Result<Task, String> {
        self.with_exclusive_lock(|| {
            input.subject = sanitize_required(&input.subject, "subject")?;
            input.description = normalize_optional_string(input.description);
            input.owner = normalize_optional_string(input.owner);
            input.active_form = normalize_optional_string(input.active_form);
            input.blocks = normalize_dependencies(input.blocks)?;
            input.blocked_by = normalize_dependencies(input.blocked_by)?;

            let now = now_unix_seconds()?;
            let task = Task {
                id: format!("task-{}", Uuid::now_v7()),
                subject: input.subject,
                description: input.description,
                status: TaskStatus::Pending,
                owner: input.owner,
                active_form: input.active_form,
                blocks: input.blocks,
                blocked_by: input.blocked_by,
                metadata: input.metadata,
                created_at: now,
                updated_at: now,
            };
            self.write_task(&task)?;
            Ok(task)
        })
    }

    pub(crate) fn list_tasks(&self) -> Result<Vec<Task>, String> {
        self.with_shared_lock(|| {
            self.ensure_team_dir()?;
            let mut tasks = Vec::new();
            let entries = fs::read_dir(&self.team_dir)
                .map_err(|err| format!("failed to read {}: {err}", self.team_dir.display()))?;
            for entry in entries {
                let Ok(entry) = entry else {
                    continue;
                };
                let path = entry.path();
                if !path.is_file() {
                    continue;
                }
                if path.file_name().is_some_and(|name| name == LOCK_FILENAME) {
                    continue;
                }
                if path.extension().and_then(|ext| ext.to_str()) != Some("json") {
                    continue;
                }
                let payload = fs::read_to_string(&path)
                    .map_err(|err| format!("failed to read {}: {err}", path.display()))?;
                let task: Task = serde_json::from_str(&payload)
                    .map_err(|err| format!("failed to parse {}: {err}", path.display()))?;
                tasks.push(task);
            }
            tasks.sort_by(|left, right| {
                left.created_at
                    .cmp(&right.created_at)
                    .then_with(|| left.id.cmp(&right.id))
            });
            Ok(tasks)
        })
    }

    pub(crate) fn get_task(&self, task_id: &str) -> Result<Option<Task>, String> {
        self.with_shared_lock(|| {
            let path = self.task_path(task_id);
            if !path.exists() {
                return Ok(None);
            }
            let payload = fs::read_to_string(&path)
                .map_err(|err| format!("failed to read {}: {err}", path.display()))?;
            let task = serde_json::from_str(&payload)
                .map_err(|err| format!("failed to parse {}: {err}", path.display()))?;
            Ok(Some(task))
        })
    }

    pub(crate) fn update_task(
        &self,
        task_id: &str,
        update: TaskUpdate,
    ) -> Result<Option<Task>, String> {
        self.with_exclusive_lock(|| {
            let path = self.task_path(task_id);
            if !path.exists() {
                return Ok(None);
            }

            let payload = fs::read_to_string(&path)
                .map_err(|err| format!("failed to read {}: {err}", path.display()))?;
            let mut task: Task = serde_json::from_str(&payload)
                .map_err(|err| format!("failed to parse {}: {err}", path.display()))?;

            if let Some(status) = update.status {
                task.status = status;
            }
            if let Some(owner) = update.owner {
                task.owner = normalize_optional_string(owner);
            }
            if let Some(subject) = update.subject {
                task.subject = sanitize_required(&subject, "subject")?;
            }
            if let Some(description) = update.description {
                task.description = normalize_optional_string(description);
            }
            if let Some(active_form) = update.active_form {
                task.active_form = normalize_optional_string(active_form);
            }
            if let Some(blocks) = update.blocks {
                task.blocks = normalize_dependencies(blocks)?;
            }
            if let Some(blocked_by) = update.blocked_by {
                task.blocked_by = normalize_dependencies(blocked_by)?;
            }
            if let Some(metadata) = update.metadata {
                task.metadata = metadata;
            }
            task.updated_at = now_unix_seconds()?;

            self.write_task(&task)?;
            Ok(Some(task))
        })
    }

    fn with_exclusive_lock<T>(
        &self,
        work: impl FnOnce() -> Result<T, String>,
    ) -> Result<T, String> {
        self.with_lock(false, work)
    }

    fn with_shared_lock<T>(&self, work: impl FnOnce() -> Result<T, String>) -> Result<T, String> {
        self.with_lock(true, work)
    }

    fn with_lock<T>(
        &self,
        shared: bool,
        work: impl FnOnce() -> Result<T, String>,
    ) -> Result<T, String> {
        self.ensure_team_dir()?;

        let lock_path = self.team_dir.join(LOCK_FILENAME);
        let lock_file = OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(&lock_path)
            .map_err(|err| format!("failed to open lock file {}: {err}", lock_path.display()))?;

        for _ in 0..LOCK_RETRIES {
            let lock_result = if shared {
                lock_file.try_lock_shared().map_err(std::io::Error::from)
            } else {
                lock_file.try_lock().map_err(std::io::Error::from)
            };
            match lock_result {
                Ok(()) => {
                    let result = work();
                    return result;
                }
                Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
                    std::thread::sleep(LOCK_RETRY_SLEEP);
                }
                Err(err) => {
                    return Err(format!(
                        "failed to lock task store {}: {err}",
                        lock_path.display()
                    ));
                }
            }
        }

        Err(format!(
            "could not acquire task store lock {} after {} retries",
            lock_path.display(),
            LOCK_RETRIES
        ))
    }

    fn ensure_team_dir(&self) -> Result<(), String> {
        fs::create_dir_all(&self.team_dir)
            .map_err(|err| format!("failed to create {}: {err}", self.team_dir.display()))
    }

    fn write_task(&self, task: &Task) -> Result<(), String> {
        let path = self.task_path(&task.id);
        let payload = serde_json::to_vec_pretty(task)
            .map_err(|err| format!("failed to serialize task {}: {err}", task.id))?;
        fs::write(&path, payload)
            .map_err(|err| format!("failed to write {}: {err}", path.display()))
    }

    fn task_path(&self, task_id: &str) -> PathBuf {
        self.team_dir.join(format!("{task_id}.json"))
    }
}

fn now_unix_seconds() -> Result<i64, String> {
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|err| format!("system clock error: {err}"))?;
    i64::try_from(duration.as_secs()).map_err(|err| format!("timestamp overflow: {err}"))
}

fn sanitize_required(value: &str, field: &str) -> Result<String, String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(format!("{field} cannot be empty"));
    }
    Ok(trimmed.to_string())
}

fn normalize_optional_string(value: Option<String>) -> Option<String> {
    value.and_then(|raw| {
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            return None;
        }
        Some(trimmed.to_string())
    })
}

fn normalize_dependencies(values: Vec<String>) -> Result<Vec<String>, String> {
    let mut normalized = Vec::new();
    let mut seen = HashSet::new();
    for value in values {
        let trimmed = value.trim();
        if trimmed.is_empty() {
            return Err("dependency values cannot be empty".to_string());
        }
        if seen.insert(trimmed.to_string()) {
            normalized.push(trimmed.to_string());
        }
    }
    Ok(normalized)
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use tempfile::tempdir;

    #[test]
    fn create_list_get_update_round_trip() {
        let home = tempdir().expect("tempdir");
        let store = TaskStore::new(home.path(), "team-1", "Alpha Team");

        let created = store
            .create_task(NewTask {
                subject: "Implement persistence".to_string(),
                description: Some("Write config.json to disk".to_string()),
                owner: Some("lead".to_string()),
                active_form: Some("implementing".to_string()),
                blocks: vec!["task-2".to_string()],
                blocked_by: vec![],
                metadata: BTreeMap::from([("priority".to_string(), Value::from("high"))]),
            })
            .expect("task should be created");

        assert_eq!(created.status, TaskStatus::Pending);
        assert_eq!(created.subject, "Implement persistence");

        let listed = store.list_tasks().expect("list tasks");
        assert_eq!(listed, vec![created.clone()]);

        let fetched = store
            .get_task(&created.id)
            .expect("get task")
            .expect("task should exist");
        assert_eq!(fetched, created);

        let updated = store
            .update_task(
                &fetched.id,
                TaskUpdate {
                    status: Some(TaskStatus::InProgress),
                    owner: Some(Some("worker-1".to_string())),
                    subject: Some("Implement team persistence".to_string()),
                    description: Some(Some("Persist teams under ~/.codex/teams".to_string())),
                    active_form: Some(None),
                    blocks: Some(vec!["task-3".to_string(), "task-3".to_string()]),
                    blocked_by: Some(vec!["task-1".to_string()]),
                    metadata: Some(BTreeMap::from([(
                        "priority".to_string(),
                        Value::from("critical"),
                    )])),
                },
            )
            .expect("update task")
            .expect("task should exist");

        assert_eq!(updated.status, TaskStatus::InProgress);
        assert_eq!(updated.owner, Some("worker-1".to_string()));
        assert_eq!(updated.subject, "Implement team persistence");
        assert_eq!(updated.active_form, None);
        assert_eq!(updated.blocks, vec!["task-3".to_string()]);
        assert_eq!(updated.blocked_by, vec!["task-1".to_string()]);
        assert_eq!(
            updated.metadata.get("priority"),
            Some(&Value::from("critical"))
        );
        assert!(updated.updated_at >= updated.created_at);
    }

    #[test]
    fn create_rejects_empty_subject() {
        let home = tempdir().expect("tempdir");
        let store = TaskStore::new(home.path(), "team-1", "alpha");

        let result = store.create_task(NewTask {
            subject: "   ".to_string(),
            description: None,
            owner: None,
            active_form: None,
            blocks: vec![],
            blocked_by: vec![],
            metadata: BTreeMap::new(),
        });
        assert_eq!(result, Err("subject cannot be empty".to_string()));
    }

    #[test]
    fn update_missing_task_returns_none() {
        let home = tempdir().expect("tempdir");
        let store = TaskStore::new(home.path(), "team-1", "alpha");

        let updated = store
            .update_task(
                "task-missing",
                TaskUpdate {
                    status: Some(TaskStatus::Completed),
                    ..TaskUpdate::default()
                },
            )
            .expect("update should succeed");
        assert_eq!(updated, None);
    }

    #[test]
    fn teams_with_same_name_use_separate_task_directories() {
        let home = tempdir().expect("tempdir");
        let alpha_one = TaskStore::new(home.path(), "team-1", "alpha");
        let alpha_two = TaskStore::new(home.path(), "team-2", "alpha");

        let first = alpha_one
            .create_task(NewTask {
                subject: "task one".to_string(),
                description: None,
                owner: None,
                active_form: None,
                blocks: vec![],
                blocked_by: vec![],
                metadata: BTreeMap::new(),
            })
            .expect("task should be created");
        let second = alpha_two
            .create_task(NewTask {
                subject: "task two".to_string(),
                description: None,
                owner: None,
                active_form: None,
                blocks: vec![],
                blocked_by: vec![],
                metadata: BTreeMap::new(),
            })
            .expect("task should be created");

        let one_tasks = alpha_one.list_tasks().expect("list tasks");
        let two_tasks = alpha_two.list_tasks().expect("list tasks");
        assert_eq!(one_tasks, vec![first]);
        assert_eq!(two_tasks, vec![second]);
    }

    #[test]
    fn list_returns_empty_when_no_tasks_exist() {
        let home = tempdir().expect("tempdir");
        let store = TaskStore::new(home.path(), "team-1", "alpha");

        let listed = store.list_tasks().expect("list should succeed");
        assert_eq!(listed, Vec::<Task>::new());
    }
}
