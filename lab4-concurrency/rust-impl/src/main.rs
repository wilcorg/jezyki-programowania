use parking_lot::Mutex;
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use rand::Rng;

const PHILOSOPHER_COUNT: usize = 5;
const DISH_COUNT: usize = 3;
const FORK_WAIT_TIMEOUT: Duration = Duration::from_millis(1000);

struct Philosopher {
    number: usize,
    left_fork: Arc<Mutex<()>>,
    right_fork: Arc<Mutex<()>>,
    dish_eaten: usize,
}

impl Philosopher {
    fn new(number: usize, left_fork: Arc<Mutex<()>>, right_fork: Arc<Mutex<()>>) -> Philosopher {
        Philosopher {
            number,
            left_fork,
            right_fork,
            dish_eaten: 0,
        }
    }

    fn eat(&mut self) {
        while self.dish_eaten < DISH_COUNT {
            println!("Philosopher #{} is thinking", self.number);
            thread::sleep(Duration::from_millis(100 * (rand::thread_rng().gen_range(1..=10))));

            println!("Philosopher #{} is hungry", self.number);
            let left_fork = self.left_fork.lock();

            // try to lock (take) fork in specified time
            // if succeeded (Some), a philosopher can consume dish
            // if failed (None), a philosopher puts their left fork back
            match self.right_fork.try_lock_for(FORK_WAIT_TIMEOUT) {
                Some(_data) => {
                    println!("Philosopher #{} is eating", self.number);
                    self.dish_eaten += 1;
                }
                None => {
                    println!("Philosopher #{} forgot why they took a fork and put it back", self.number);
                }
            }

            thread::sleep(Duration::from_millis(100 * (rand::thread_rng().gen_range(1..=10))));
            drop(left_fork);
        }
        println!("Philosopher #{} is leaving", self.number);
    }
}

fn main() {
    let forks: Vec<_> = (0..PHILOSOPHER_COUNT).map(|_| Arc::new(Mutex::new(()))).collect();
    let mut tasks = vec![];

    let philosophers: Vec<_> = (0..PHILOSOPHER_COUNT).map(|i| {
        Philosopher::new(i + 1, forks[i].clone(), forks[(i + 1) % PHILOSOPHER_COUNT].clone())
    }).collect();

    for mut philosopher in philosophers {
        let task = thread::spawn(move || {
            philosopher.eat();
        });
        tasks.push(task);
    }

    for task in tasks {
        task.join().unwrap();
    }
}