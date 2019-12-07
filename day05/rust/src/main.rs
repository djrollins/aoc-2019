use std::fs;
use std::path::Path;

fn main() {
    let input_file = std::env::args().nth(1).unwrap();
    let input = fs::read_to_string(Path::new(&input_file)).unwrap();
    let memory = input
        .split(',')
        .filter_map(|s| s.parse().ok())
        .collect::<Vec<isize>>();

    println!("part1: {}", run(&memory, std::iter::once(1)));
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
        position: usize,
        fst: Parameter,
        snd: Parameter,
    },
    Mult {
        position: usize,
        fst: Parameter,
        snd: Parameter,
    },
    Read {
        position: usize,
    },
    Output {
        position: Parameter,
    },
    Terminate,
}

impl Op {
    fn exec<I>(&self, mem: &mut [isize], input: &mut I, output: &mut isize) -> usize
    where
        I: Iterator<Item = isize>,
    {
        match self {
            Op::Terminate => panic!("ERROR: tried to execute a Terminate instruction"),
            Op::Add { position, fst, snd } => {
                mem[*position] = fst.resolve(&mem) + snd.resolve(&mem)
            }
            Op::Mult { position, fst, snd } => {
                mem[*position] = fst.resolve(&mem) * snd.resolve(&mem)
            }
            Op::Read { position } => mem[*position] = input.next().unwrap(),
            Op::Output { position } => *output = position.resolve(&mem),
        }

        self.jump()
    }

    fn jump(&self) -> usize {
        match self {
            Op::Add { .. } => 4,
            Op::Mult { .. } => 4,
            Op::Read { .. } => 2,
            Op::Output { .. } => 2,
            Op::Terminate => unreachable!(),
        }
    }
}

fn run<I>(memory: &[isize], mut input: I) -> isize
where
    I: Iterator<Item = isize>,
{
    let mut memory = memory.to_owned();

    let mut pos: usize = 0;
    let mut output = 0;

    loop {
        let op = op(pos, &memory);
        match op {
            Op::Terminate => break output,
            _ => {
                assert_eq!(output, 0, "pos = {}", pos);
                pos += op.exec(&mut memory, &mut input, &mut output);
            }
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

fn op(pos: usize, array: &[isize]) -> Op {
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
        3 => Op::Read {
            position: array[pos + 1] as usize,
        },
        4 => Op::Output {
            position: param(mode1, array[pos + 1]),
        },
        _ => panic!("ERROR: could not parse op_code {}", op_code),
    }
}
