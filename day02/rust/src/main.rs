use std::fs;
use std::path::Path;

fn main() {
    let input_file = std::env::args().nth(1).unwrap();
    let input = fs::read_to_string(Path::new(&input_file)).unwrap();
    let array = input
        .split(',')
        .filter_map(|s| s.parse().ok())
        .collect::<Vec<usize>>(); 

    println!("part1: {}", run(&array, 12, 2));

    for n in 0..=99 {
        for v in 0..=99 {
            if run(&array, n, v) == 19690720 {
                println!("part2: {}", (100 * n) + v);
                break;
            }
        }
    }
}

fn run(input: &[usize], fst: usize, snd: usize) -> usize {
    let mut array = input.to_owned();

    let mut pos: usize = 0;

    array[1] = fst;
    array[2] = snd;

    loop {
        match array[pos] {
            1 => {
                let (dst, fst, snd) = op(&array, pos);
                array[dst] = fst + snd;
            },
            2 => {
                let (dst, fst, snd) = op(&array, pos);
                array[dst] = fst * snd;
            },
            99 => {
                break;
            },
            _ => unreachable!(),
        }

        pos += 4;
    }

    array[0]
}

fn op(array: &[usize], pos: usize) -> (usize, usize, usize) {
    let dst = array[pos + 3];
    let fst = array[array[pos + 1]];
    let snd = array[array[pos + 2]];
    (dst, fst, snd)
}
