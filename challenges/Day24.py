## Couldn't make z3 work for haskell so here we are...

from z3 import *

def solve(instructions):
    solver = z3.Optimize()

    zero, one = z3.BitVecVal(0, 64), z3.BitVecVal(1, 64)

    digits = [z3.BitVec(f'd_{i}', 64) for i in range(14)]

    for d in digits:
        solver.add(z3.And(1 <= d, d <= 9))

    digit_input = iter(digits)

    digits_base_10 = z3.BitVec('digits_base_10', 64)
    solver.add(digits_base_10 == sum((10 ** i) * d for i, d in enumerate(digits[::-1])))

    registers = { r: zero for r in 'xyzw' }

    for i, instruction in enumerate(instructions):
        if instruction[0] == 'inp':
            registers[instruction[1]] = next(digit_input)
        else:
            register, operand = instruction[1:]
            operand = registers[operand] if operand in registers else int(operand)
            instruction_i = z3.BitVec(f'instruction_{i}', 64)
            match instruction[0]:
                case 'add':
                    solver.add(instruction_i == registers[register] + operand)
                case 'mul':
                    solver.add(instruction_i == registers[register] * operand)
                case 'mod':
                    solver.add(registers[register] >= 0)
                    solver.add(operand > 0)
                    solver.add(instruction_i == registers[register] % operand)
                case 'div':
                    solver.add(operand != 0)
                    solver.add(instruction_i == registers[register] / operand)
                case 'eql':
                    solver.add(instruction_i == z3.If(registers[register] == operand, one, zero))
            registers[register] = instruction_i
    
    solver.add(registers['z'] == 0)

    for goal in [solver.maximize, solver.minimize]:
        solver.push()
        goal(digits_base_10)
        assert(solver.check() == z3.sat)
        print(solver.model().eval(digits_base_10))
        solver.pop()
            



def read_input(raw):
    return list(map(lambda line: line.split(' '), raw.split("\n")))

def main():
    with open("../input/24.txt") as file:
        input = read_input(file.read())
        solve(input)

if __name__ == "__main__":
    main()