use std::cmp::Ordering;
use std::fs;
use std::path::Path;

fn main() {
    let input_file = std::env::args().nth(1).unwrap();
    let input = fs::read_to_string(Path::new(&input_file)).unwrap();
    let memory = input
        .split(',')
        .filter_map(|x| x.trim().parse().ok())
        .collect::<Vec<isize>>();

    println!(
        "part 1: {}",
        Intcode::new(&memory).run(&mut std::iter::once(1)).unwrap()
    );
    println!(
        "part 2: {}",
        Intcode::new(&memory).run(&mut std::iter::once(5)).unwrap()
    );
}

struct Intcode {
    inst_ptr: usize,
    memory: Box<[isize]>,
    output: Option<isize>,
}

impl Intcode {
    fn new(memory: &[isize]) -> Intcode {
        Intcode {
            memory: memory.into(),
            inst_ptr: 0,
            output: None,
        }
    }

    fn run<I>(&mut self, input: &mut I) -> Option<isize>
    where
        I: Iterator<Item = isize>,
    {
        loop {
            let op = Op::new(self.inst_ptr, &self.memory);
            let mut jump = op.jump();
            match op {
                Op::Terminate => break self.output,
                Op::Add { position, fst, snd } => {
                    self.memory[position] = fst.resolve(&self.memory) + snd.resolve(&self.memory)
                }
                Op::Mult { position, fst, snd } => {
                    self.memory[position] = fst.resolve(&self.memory) * snd.resolve(&self.memory)
                }
                Op::ReadInput { position } => self.memory[position] = input.next().unwrap(),
                Op::Output { position } => self.output = Some(position.resolve(&self.memory)),
                Op::JumpIf { condition, value, position } => {
                    if (value.resolve(&self.memory) != 0) == condition {
                        self.inst_ptr = position.resolve(&self.memory) as usize;
                        jump = 0;
                    }
                }
                Op::Compare {
                    ordering,
                    fst,
                    snd,
                    position,
                } => {
                    self.memory[position] =
                        if fst.resolve(&self.memory).cmp(&snd.resolve(&self.memory)) == ordering {
                            1
                        } else {
                            0
                        }
                }
            }
            self.inst_ptr += jump;
        }
    }
}

#[derive(Debug)]
enum Parameter {
    Immediate(isize),
    Position(usize),
}

impl Parameter {
    fn resolve(&self, input: &[isize]) -> isize {
        match self {
            Parameter::Immediate(val) => *val,
            Parameter::Position(pos) => input[*pos],
        }
    }
}

#[derive(Debug)]
enum Op {
    Add {
        fst: Parameter,
        snd: Parameter,
        position: usize,
    },
    Mult {
        fst: Parameter,
        snd: Parameter,
        position: usize,
    },
    ReadInput {
        position: usize,
    },
    Output {
        position: Parameter,
    },
    JumpIf {
        condition: bool,
        value: Parameter,
        position: Parameter,
    },
    Compare {
        ordering: Ordering,
        fst: Parameter,
        snd: Parameter,
        position: usize,
    },
    Terminate,
}

impl Op {
    fn new(pos: usize, array: &[isize]) -> Op {
        let inst = array[pos];
        let _mode3 = (inst / 10000) % 10 != 0;
        let mode2 = (inst / 1000) % 10 != 0;
        let mode1 = (inst / 100) % 10 != 0;
        let op_code = inst % 100;

        match op_code {
            99 => Op::Terminate,
            1 => Op::Add {
                fst: param(mode1, array[pos + 1]),
                snd: param(mode2, array[pos + 2]),
                position: array[pos + 3] as usize,
            },
            2 => Op::Mult {
                fst: param(mode1, array[pos + 1]),
                snd: param(mode2, array[pos + 2]),
                position: array[pos + 3] as usize,
            },
            3 => Op::ReadInput {
                position: array[pos + 1] as usize,
            },
            4 => Op::Output {
                position: param(mode1, array[pos + 1]),
            },
            5 | 6 => Op::JumpIf {
                condition: op_code == 5,
                value: param(mode1, array[pos + 1]),
                position: param(mode2, array[pos + 2]),
            },
            7 | 8 => Op::Compare {
                ordering: if op_code == 7 {
                    Ordering::Less
                } else {
                    Ordering::Equal
                },
                fst: param(mode1, array[pos + 1]),
                snd: param(mode2, array[pos + 2]),
                position: array[pos + 3] as usize,
            },
            _ => panic!("ERROR: could not parse op_code {}", op_code),
        }
    }

    fn jump(&self) -> usize {
        match self {
            Op::Add { .. } => 4,
            Op::Mult { .. } => 4,
            Op::ReadInput { .. } => 2,
            Op::Output { .. } => 2,
            Op::JumpIf { .. } => 3,
            Op::Compare { .. } => 4,
            Op::Terminate => 0,
        }
    }
}

fn param(immediate: bool, val: isize) -> Parameter {
    if immediate {
        Parameter::Immediate(val)
    } else {
        Parameter::Position(val as usize)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn equal_in_position_mode() {
        let memory = "3,9,8,9,10,9,4,9,99,-1,8"
            .split(',')
            .filter_map(|s| s.parse().ok())
            .collect::<Vec<isize>>();

        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(7)), Some(0));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(8)), Some(1));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(9)), Some(0));
    }

    #[test]
    fn equal_in_immediate_mode() {
        let memory = "3,3,1108,-1,8,3,4,3,99"
            .split(',')
            .filter_map(|s| s.parse().ok())
            .collect::<Vec<isize>>();

        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(7)), Some(0));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(8)), Some(1));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(9)), Some(0));
    }

    #[test]
    fn less_than_in_position_mode() {
        let memory = "3,9,7,9,10,9,4,9,99,-1,8"
            .split(',')
            .filter_map(|s| s.parse().ok())
            .collect::<Vec<isize>>();

        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(7)), Some(1));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(8)), Some(0));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(9)), Some(0));
    }

    #[test]
    fn less_than_in_immediate_mode() {
        let memory = "3,3,1107,-1,8,3,4,3,99"
            .split(',')
            .filter_map(|s| s.parse().ok())
            .collect::<Vec<isize>>();

        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(7)), Some(1));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(8)), Some(0));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(9)), Some(0));
    }

    #[test]
    fn jump_if_true_in_position_mode() {
        let memory = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
            .split(',')
            .filter_map(|s| s.parse().ok())
            .collect::<Vec<isize>>();

        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(0)), Some(0));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(1)), Some(1));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(9)), Some(1));
    }

    #[test]
    fn jump_if_true_in_immediate_mode() {
        let memory = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
            .split(',')
            .filter_map(|s| s.parse().ok())
            .collect::<Vec<isize>>();

        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(0)), Some(0));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(1)), Some(1));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(9)), Some(1));
    }

    #[test]
    fn larger_example() {
        let memory = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
            .split(',')
            .filter_map(|s| s.parse().ok())
            .collect::<Vec<isize>>();

        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(7)), Some(999));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(8)), Some(1000));
        assert_eq!(Intcode::new(&memory).run(&mut std::iter::once(9)), Some(1001));
    }
}
