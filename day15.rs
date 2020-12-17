use anyhow::Result;
use std::collections::HashMap;
fn main() -> Result<()> {
    eprintln!("{:?}", Game::new(&[9,6,0,10,18,2,1]).epoch(30000000));
    Ok(())
}


struct Game {
    epoch: usize,
    x: usize,
    last: HashMap<usize, usize>,
    previous: HashMap<usize, usize>
}

impl Game {
    pub fn new(start: &[usize]) -> Self {
        let mut game = Game {
            epoch: 0, 
            x: 0,
            last: HashMap::new(),
            previous: HashMap::new()
        };

        for v in start {
            game.add(*v);
        }

        game
    }

    fn add(&mut self, v: usize) {
        self.last.insert(v, self.epoch).and_then(|e2| self.previous.insert(v, e2));
        self.epoch += 1;
        self.x = v;
    }

    fn epoch(mut self, t: usize) -> usize {
        self.nth(t - self.epoch - 1).unwrap()
    }

}

impl Iterator for Game {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let n: usize = match self.previous.get(&self.x) {
            Some(t1) => self.last[&self.x] - t1,
            None => 0,
        };
        self.add(n);
        Some(n)
    }
}