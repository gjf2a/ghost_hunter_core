#![cfg_attr(not(test), no_std)]

use core::ops::{Add,Sub};
use pc_keyboard::{DecodedKey, KeyCode};

const UPDATE_FREQUENCY: usize = 3;
const EMPOWER_TICKS: usize = 60;

#[derive(Copy,Debug,Clone,Eq,PartialEq)]
pub struct GhostHunterGame<const WIDTH: usize, const HEIGHT: usize> {
    cells: [[Cell; WIDTH]; HEIGHT],
    ghost_hunter: GhostHunter<WIDTH,HEIGHT>,
    ghosts: [Ghost<WIDTH,HEIGHT>; 4],
    status: Status,
    dots_eaten: u32,
    countdown: usize,
    last_key: Option<Dir>,
    empowered_ticks_left: usize
}

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
#[repr(u8)]
pub enum Dir {
    N, S, E, W
}

impl Dir {
    fn icon(&self) -> char {
        match self {
            Dir::N => 'v',
            Dir::S => '^',
            Dir::E => '<',
            Dir::W => '>'
        }
    }

    fn reverse(&self) -> Dir {
        match self {
            Dir::N => Dir::S,
            Dir::S => Dir::N,
            Dir::E => Dir::W,
            Dir::W => Dir::E
        }
    }

    fn left(&self) -> Dir {
        match self {
            Dir::N => Dir::W,
            Dir::S => Dir::E,
            Dir::E => Dir::N,
            Dir::W => Dir::S
        }
    }

    fn right(&self) -> Dir {
        match self {
            Dir::N => Dir::E,
            Dir::S => Dir::W,
            Dir::E => Dir::S,
            Dir::W => Dir::N
        }
    }
}

impl From<char> for Dir {
    fn from(icon: char) -> Self {
        match icon {
            '^' => Dir::S,
            'v' => Dir::N,
            '>' => Dir::W,
            '<' => Dir::E,
            _ => panic!("Illegal icon: '{}'", icon)
        }
    }
}

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
#[repr(u8)]
pub enum Cell {
    Dot,
    Empty,
    Wall,
    PowerDot
}

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
pub struct Position<const WIDTH: usize, const HEIGHT: usize> {
    col: i16, row: i16
}

impl <const WIDTH: usize, const HEIGHT: usize> Add for Position<WIDTH,HEIGHT> {
    type Output = Position<WIDTH,HEIGHT>;

    fn add(self, rhs: Self) -> Self::Output {
        Position {col: self.col + rhs.col, row: self.row + rhs.row}
    }
}

impl <const WIDTH: usize, const HEIGHT: usize> Sub for Position<WIDTH,HEIGHT> {
    type Output = Position<WIDTH,HEIGHT>;

    fn sub(self, rhs: Self) -> Self::Output {
        Position {col: self.col - rhs.col, row: self.row - rhs.row}
    }
}

impl <const WIDTH: usize, const HEIGHT: usize> Position<WIDTH,HEIGHT> {
    pub fn is_legal(&self) -> bool {
        0 <= self.col && self.col < WIDTH as i16 && 0 <= self.row && self.row < HEIGHT as i16
    }

    pub fn row_col(&self) -> (usize, usize) {
        (self.row as usize, self.col as usize)
    }

    pub fn neighbor(&self, d: Dir) -> Position<WIDTH,HEIGHT> {
        match d {
            Dir::N => Position {row: self.row - 1, col: self.col},
            Dir::S => Position {row: self.row + 1, col: self.col},
            Dir::E => Position {row: self.row,     col: self.col + 1},
            Dir::W => Position {row: self.row,     col: self.col - 1}
        }
    }
}

#[derive(Copy,Clone,Eq,PartialEq,Debug)]
struct GhostHunter<const WIDTH: usize, const HEIGHT: usize> {
    pos: Position<WIDTH,HEIGHT>, dir: Dir, open: bool
}

impl <const WIDTH: usize, const HEIGHT: usize> GhostHunter<WIDTH,HEIGHT> {
    fn new(pos: Position<WIDTH,HEIGHT>, icon: char) -> Self {
        GhostHunter {pos, dir: Dir::from(icon), open: true}
    }

    fn tick(&mut self) {
        self.open = !self.open;
    }

    fn icon(&self) -> char {
        if self.open {
            self.dir.icon()
        } else {
            match self.dir {
                Dir::N | Dir::S => '|',
                Dir::E | Dir::W => '-'
            }
        }
    }
}

#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub struct Ghost<const WIDTH: usize, const HEIGHT: usize> {
    pos: Position<WIDTH,HEIGHT>, dir: Dir, active: bool
}

impl <const WIDTH: usize, const HEIGHT: usize> Ghost<WIDTH,HEIGHT> {
    fn on_my_left(&self, other: Position<WIDTH,HEIGHT>) -> bool {
        let offset = self.pos - other;
        match self.dir {
            Dir::N => offset.col > 0,
            Dir::S => offset.col < 0,
            Dir::E => offset.row > 0,
            Dir::W => offset.row < 0
        }
    }

    fn ahead_or_behind(&self, other: Position<WIDTH,HEIGHT>) -> bool {
        let offset = self.pos - other;
        match self.dir {
            Dir::N | Dir::S => offset.col == 0,
            Dir::E | Dir::W => offset.row == 0
        }
    }

    fn on_my_right(&self, other: Position<WIDTH,HEIGHT>) -> bool {
        !self.on_my_left(other) && !self.ahead_or_behind(other)
    }

    fn go(&mut self, ahead: Cell, left: Cell, right: Cell, ghost_hunter_pos: Position<WIDTH,HEIGHT>) {
        if self.active {
            if left == Cell::Wall && ahead == Cell::Wall && right == Cell::Wall {
                self.dir = self.dir.reverse();
            } else if left != Cell::Wall && (self.on_my_left(ghost_hunter_pos) || ahead == Cell::Wall) {
                self.dir = self.dir.left();
            } else if right != Cell::Wall && (self.on_my_right(ghost_hunter_pos) || ahead == Cell::Wall) {
                self.dir = self.dir.right();
            }
            self.pos = self.pos.neighbor(self.dir);
        }
    }

    pub fn icon(&self) -> char {
        if self.active {'A'} else {'*'}
    }

    fn squash(&mut self) {
        self.active = false;
    }

    fn revive(&mut self) {
        self.active = true;
    }
}


#[derive(Copy,Clone,Eq,PartialEq,Debug)]
pub enum Status {
    Normal,
    Over,
    Empowered
}

const GHOST_START_DIR: [Dir; 4] = [Dir::E, Dir::W, Dir::E, Dir::W];

const START: &'static str =
    "################################################################################
     #.........A............................................................A.......#
     #.#################.##.##.###.####.#.##############.##.##.##.##.################
     #.#################.##.##.###.####.#.##############.##.##.##.##.################
     #.#################.##.##.###.####.#.##############.##.##.##.##.################
     #.......O.........................................................O............#
     ###.####.#####.######.####.#.#.#######.#.####.####.#.######.#.####.###.###.##.##
     ###.####.#####.######.####.#.#.#######.#.####.####.#.######.#.####.###.###.##.##
     ###.####.#####.######.####.#.#.#######.#.####.####.#.######.#.####.###.###.##.##
     ###.####.#####.######.####.#.#.#######.#.####.####.#.######.#.####.###.###.##.##
     #......................................<.......................................#
     #####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.##
     #####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.##
     #####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.##
     #####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.##
     #####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.##
     #####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.#####.##
     #........O...........................................................O.........#
     ####.####.####.####.####.####.####.####.####.####.####.####.####.####.####.##.##
     ####.####.####.####.####.####.####.####.####.####.####.####.####.####.####.##.##
     ####.####.####.####.####.####.####.####.####.####.####.####.####.####.####.##.##
     #.........A............................................................A.......#
     ################################################################################";

impl <const WIDTH: usize, const HEIGHT: usize> GhostHunterGame<WIDTH, HEIGHT> {
    pub fn new() -> Self {
        let mut game = GhostHunterGame {
            cells: [[Cell::Dot; WIDTH]; HEIGHT],
            ghost_hunter: GhostHunter::new(Position { col: 0, row: 0}, '>'),
            ghosts: [Ghost { pos: Position {col: 0, row: 0}, dir: Dir::N, active: true }; 4],
            dots_eaten: 0, countdown: UPDATE_FREQUENCY, last_key: None, status: Status::Normal,
            empowered_ticks_left: 0
        };
        game.reset();
        game
    }

    fn reset(&mut self) {
        let mut ghost = 0;
        for (row, row_chars) in START.split('\n').enumerate() {
            for (col, icon) in row_chars.trim().chars().enumerate() {
                self.translate_icon(&mut ghost, row, col, icon);
            }
        }
        assert_eq!(ghost, 4);
        self.status = Status::Normal;
        self.dots_eaten = 0;
        self.last_key = None;
        self.empowered_ticks_left = 0;
    }

    pub fn score(&self) -> u32 {
        self.dots_eaten
    }

    fn translate_icon(&mut self, ghost: &mut usize, row: usize, col: usize, icon: char) {
        match icon {
            '#' => self.cells[row][col] = Cell::Wall,
            '.' => self.cells[row][col] = Cell::Dot,
            'A' => {
                let dir = GHOST_START_DIR[*ghost];
                self.ghosts[*ghost] = Ghost {pos: Position {row: row as i16, col: col as i16}, dir, active: true};
                *ghost += 1;
            },
            'O' => self.cells[row][col] = Cell::PowerDot,
            '>' |'<' | '^' | 'v' => {
                self.ghost_hunter = GhostHunter::new(Position {row: row as i16, col: col as i16}, icon);
            },
            _ =>  panic!("Unrecognized character: '{}'", icon)
        }
    }

    pub fn cell(&self, p: Position<WIDTH,HEIGHT>) -> Cell {
        self.cells[p.row as usize][p.col as usize]
    }

    pub fn cell_pos_iter(&self) -> RowColIter<WIDTH,HEIGHT> {
        RowColIter { row: 0, col: 0 }
    }

    pub fn ghost_hunter_at(&self) -> Position<WIDTH,HEIGHT> {
        self.ghost_hunter.pos
    }

    pub fn ghost_hunter_icon(&self) -> char {
        self.ghost_hunter.icon()
    }

    pub fn ghost_at(&self, p: Position<WIDTH,HEIGHT>) -> Option<(usize,&Ghost<WIDTH,HEIGHT>)> {
        self.ghosts.iter().enumerate().find(|(_, ghost)| ghost.pos == p)
    }

    pub fn update(&mut self) {
        self.resolve_move();
        self.last_key = None;
        self.ghost_hunter.tick();
        self.empower_tick();
        self.update_ghosts();
    }

    fn empower_tick(&mut self) {
        if self.empowered_ticks_left > 0 {
            self.empowered_ticks_left -= 1;
            if self.empowered_ticks_left == 0 {
                self.status = Status::Normal;
                for ghost in self.ghosts.iter_mut() {
                    ghost.revive();
                }
            }
        }
    }

    fn update_ghosts(&mut self) {
        for g in 0..self.ghosts.len() {
            let (ahead, left, right) = self.ahead_left_right(self.ghosts[g].pos, self.ghosts[g].dir);
            self.resolve_ghost_collision(g);
            self.ghosts[g].go(ahead, left, right, self.ghost_hunter.pos);
            self.resolve_ghost_collision(g);
        }
    }

    fn resolve_ghost_collision(&mut self, g: usize) {
        if self.ghosts[g].pos == self.ghost_hunter.pos && self.ghosts[g].active {
            match self.status {
                Status::Normal => self.status = Status::Over,
                Status::Empowered => {
                    self.dots_eaten += 100;
                    self.ghosts[g].squash();
                }
                Status::Over => {}
            }
        }
    }

    fn ahead_left_right(&self, p: Position<WIDTH,HEIGHT>, dir: Dir) -> (Cell,Cell,Cell) {
        let ahead = self.cell(p.neighbor(dir));
        let left = self.cell(p.neighbor(dir.left()));
        let right = self.cell(p.neighbor(dir.right()));
        (ahead, left, right)
    }

    pub fn countdown_complete(&mut self) -> bool {
        if self.countdown == 0 {
            self.countdown = UPDATE_FREQUENCY;
            true
        } else {
            self.countdown -= 1;
            false
        }
    }

    pub fn key(&mut self, key: DecodedKey) {
        match self.status {
            Status::Over => {
                match key {
                    DecodedKey::RawKey(KeyCode::S) | DecodedKey::Unicode('s') => self.reset(),
                    _ => {}
                }
            }
            _ => {
                let key = key2dir(key);
                if key.is_some() {
                    self.last_key = key;
                }
            }
        }
    }

    fn resolve_move(&mut self) {
        if let Some(dir) = self.last_key {
            let neighbor = self.ghost_hunter.pos.neighbor(dir);
            if neighbor.is_legal() {
                let (row, col) = neighbor.row_col();
                if self.cells[row][col] != Cell::Wall {
                    self.move_to(neighbor, dir);
                }
            }
        }
    }

    fn move_to(&mut self, neighbor: Position<WIDTH,HEIGHT>, dir: Dir) {
        self.ghost_hunter.pos = neighbor;
        self.ghost_hunter.dir = dir;
        let (row, col) = neighbor.row_col();
        match self.cells[row][col] {
            Cell::Dot => {
                self.dots_eaten += 1;
                self.cells[row][col] = Cell::Empty;
            }
            Cell::PowerDot => {
                self.dots_eaten += 10;
                self.cells[row][col] = Cell::Empty;
                self.status = Status::Empowered;
                self.empowered_ticks_left = EMPOWER_TICKS;
            }
            _ => {}
        }
    }

    pub fn status(&self) -> Status {
        self.status
    }

    pub fn empowered_ticks_left(&self) -> usize {
        self.empowered_ticks_left
    }
}

fn key2dir(key: DecodedKey) -> Option<Dir> {
    match key {
        DecodedKey::RawKey(k) => match k {
            KeyCode::ArrowUp => Some(Dir::N),
            KeyCode::ArrowDown => Some(Dir::S),
            KeyCode::ArrowLeft => Some(Dir::W),
            KeyCode::ArrowRight => Some(Dir::E),
            _ => None
        }
        DecodedKey::Unicode(c) => match c {
            'w' => Some(Dir::N),
            'a' => Some(Dir::W),
            's' => Some(Dir::S),
            'd' => Some(Dir::E),
            _ => None
        }
    }
}

pub struct RowColIter<const WIDTH: usize, const HEIGHT: usize> {
    row: usize, col: usize
}

impl <const WIDTH: usize, const HEIGHT: usize> Iterator for RowColIter<WIDTH,HEIGHT> {
    type Item = Position<WIDTH,HEIGHT>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.row < HEIGHT {
            let result = Some(Position {row: self.row as i16, col: self.col as i16});
            self.col += 1;
            if self.col == WIDTH {
                self.col = 0;
                self.row += 1;
            }
            result
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const START_WIDTH: usize = 80;
    const START_HEIGHT: usize = 23;

    fn width() -> usize {
        START.split('\n').next().unwrap().len()
    }

    fn height() -> usize {
        START.split('\n').count()
    }

    #[test]
    fn start_size() {
        // Once const expressions are more flexible, we'd use those instead.
        println!("({},{})", width(), height());
    }

    // TODO: Create a set of test cases with a much smaller game board.
    // Consider using property-based testing (e.g. https://github.com/HypothesisWorks/hypothesis)
    // This tests motion and dot eating.
    // Still need to test:
    // * Ghost collisions
    // * Power dots
    // * What happens when all dots are eaten?
    #[test]
    fn first_few_moves() {
        let mut game: GhostHunterGame<START_WIDTH,START_HEIGHT> = GhostHunterGame::new();
        let tests = [('w', 0, 0, 0), ('a', -1, 0, 1), ('a', -1, 0, 2), ('a', -1, 0, 3),
            ('a', -1, 0, 4), ('s', 0, 1, 5), ('s', 0, 1, 6), ('s', 0, 1, 7), ('s', 0, 1, 8),
            ('s', 0, 1, 9), ('s', 0, 1, 10), ('s', 0, 1, 11), ('s', 0, 0, 11), ('d', 1, 0, 12),
            ('d', 1, 0, 13), ('d', 1, 0, 14), ('d', 1, 0, 15), ('d', 1, 0, 16), ('d', 1, 0, 17),
            ('w', 0, -1, 18), ('w', 0, -1, 19), ('w', 0, -1, 20), ('w', 0, -1, 21), ('w', 0, -1, 22),
            ('w', 0, -1, 23), ('w', 0, -1, 24), ('w', 0, 0, 24)
        ];
        for (key, col_diff, row_diff, score) in tests.iter() {
            let was = game.ghost_hunter.pos;
            game.key(DecodedKey::Unicode(*key));
            game.update();
            let diff = game.ghost_hunter.pos - was;
            assert_eq!(diff.col, *col_diff);
            assert_eq!(diff.row, *row_diff);
            assert_eq!(game.dots_eaten, *score);
        }
    }

    #[test]
    fn test_ghost_ai() {
        let mut ghost: Ghost<START_WIDTH,START_HEIGHT> = Ghost {
            pos: Position {col: 10, row: 15},
            dir: Dir::N,
            active: true
        };
        assert!(ghost.on_my_left(Position {col: 8, row: 15}));
        assert!(ghost.on_my_right(Position {col: 12, row: 15}));
        assert!(!ghost.on_my_right(Position {col: 8, row: 15}));
        assert!(!ghost.on_my_left(Position {col: 12, row: 15}));
        ghost.pos.col = 1;
        ghost.dir = Dir::W;
        ghost.go(Cell::Wall, Cell::Empty, Cell::Wall, Position {col: 2, row: 15});
        assert_eq!(ghost.dir, Dir::S);
        assert_eq!(ghost.pos, Position {col: 1, row: 16});
    }

    #[test]
    fn test_above_left_right() {
        let game: GhostHunterGame<START_WIDTH,START_HEIGHT> = GhostHunterGame::new();
        let tests = [
            (Dir::W, Cell::Wall, Cell::Dot, Cell::Wall),
            (Dir::N, Cell::Wall, Cell::Wall, Cell::Dot),
            (Dir::E, Cell::Dot, Cell::Wall, Cell::Dot),
            (Dir::S, Cell::Dot, Cell::Dot, Cell::Wall)
        ];
        for (dir, ahead, left, right) in tests.iter() {
            let (a, l, r) = game.ahead_left_right(Position {col: 1, row: 1}, *dir);
            assert_eq!(a, *ahead);
            assert_eq!(l, *left);
            assert_eq!(r, *right);
        }
    }

    #[test]
    fn test_exit_screen_bug() {
        let mut game: GhostHunterGame<START_WIDTH,START_HEIGHT> = GhostHunterGame::new();
        game.ghosts[1].pos = Position {row: 1, col: 1};
        game.ghosts[1].dir = Dir::W;
        let (ahead, left, right) = game.ahead_left_right(game.ghosts[1].pos, game.ghosts[1].dir);
        game.ghosts[1].go(ahead, left, right, game.ghost_hunter.pos);
        assert_eq!(game.ghosts[1].dir, Dir::S);
        assert_eq!(game.ghosts[1].pos, Position {row: 2, col: 1});
    }
}