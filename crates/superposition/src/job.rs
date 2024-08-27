use fang::asynk::async_queue::AsyncQueueable;
use fang::serde::{Deserialize, Serialize};
use fang::typetag;
use fang::AsyncRunnable;
use fang::FangError;
use fang::{async_trait, Scheduled};
use std::time::Duration;

#[derive(Serialize, Deserialize)]
#[serde(crate = "fang::serde")]
pub struct MyTask {
    pub number: u16,
}

impl MyTask {
    pub fn new(number: u16) -> Self {
        Self { number }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(crate = "fang::serde")]
pub struct MyFailingTask {
    pub number: u16,
}

impl MyFailingTask {
    pub fn new(number: u16) -> Self {
        Self { number }
    }
}

#[async_trait]
#[typetag::serde]
impl AsyncRunnable for MyTask {
    async fn run(&self, queue: &mut dyn AsyncQueueable) -> Result<(), FangError> {
        let new_task = MyTask::new(self.number + 1);
        queue
            .insert_task(&new_task as &dyn AsyncRunnable)
            .await
            .unwrap();

        log::info!("the current number is {}", self.number);
        tokio::time::sleep(Duration::from_secs(3)).await;

        Ok(())
    }

    // this func is optional
    // Default task_type is common
    fn task_type(&self) -> String {
        "my_successful_task".to_string()
    }

    // If `uniq` is set to true and the task is already in the storage, it won't be inserted again
    // The existing record will be returned for for any insertions operaiton
    fn uniq(&self) -> bool {
        true
    }

    // This will be useful if you would like to schedule tasks.
    // default value is None (the task is not scheduled, it's just executed as soon as it's inserted)
    fn cron(&self) -> Option<Scheduled> {
        let expression = "0 30 * * * * *";
        Some(Scheduled::CronPattern(expression.to_string()))
    }

    // the maximum number of retries. Set it to 0 to make it not retriable
    // the default value is 20
    fn max_retries(&self) -> i32 {
        3
    }

    // backoff mode for retries
    fn backoff(&self, attempt: u32) -> u32 {
        u32::pow(2, attempt)
    }
}

#[async_trait]
#[typetag::serde]
impl AsyncRunnable for MyFailingTask {
    async fn run(&self, queue: &mut dyn AsyncQueueable) -> Result<(), FangError> {
        let new_task = MyFailingTask::new(self.number + 1);
        queue
            .insert_task(&new_task as &dyn AsyncRunnable)
            .await
            .unwrap();

        log::info!("the current number is {}", self.number);
        tokio::time::sleep(Duration::from_secs(3)).await;

        let b = true;

        if b {
            panic!("Hello!");
        } else {
            Ok(())
        }
    }
}
