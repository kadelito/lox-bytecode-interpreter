from typing_extensions import override
from abc import ABC, abstractmethod
from enum import Enum
from random import random, choice, randint


def main():
    for _ in range(10):
        print(randExpr())

def randExpr() -> "Expr":
    if random() < 0.5:
        return Number(randint(-100, 100))
    else:
        return randBin()

def randBin() -> "Binary":
    return Binary(
        randExpr(),
        choice(list(BinOp)),
        randExpr()
    )

def randfloat(start: float, end: float) -> float:
    return start + (end - start)*random()

class BinOp(Enum):
    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"

    def apply(self, left: float, right: float):
        match self:
            case BinOp.ADD: return left + right
            case BinOp.SUB: return left - right
            case BinOp.MUL: return left * right
            case BinOp.DIV: return left / right
        return None

    def prec(self):
        match self:
            case BinOp.ADD, BinOp.SUB: return 1
            case BinOp.MUL, BinOp.DIV: return 2
        return None

class Expr(ABC):
    @abstractmethod
    def evaluate(self) -> float:
        pass

    @abstractmethod
    def tostr(self) -> str:
        pass

    def __str__(self):
        return self.tostr()

class Number(Expr):

    def __init__(self, value: float|int):
        self.value = float(value)

    def evaluate(self) -> float:
        return self.value

    def tostr(self) -> str:
        string = str(self.value)
        if string.endswith(".0"):
            return string[:-2]
        else:
            index = string.index(".")
            return string[:index + 3]

class Binary(Expr):

    def __init__(self, left: Expr, op: BinOp, right: Expr):
        self.left = left
        self.op = op
        self.right = right

    @override
    def evaluate(self) -> float:
        left = self.left.evaluate()
        right = self.left.evaluate()
        return self.op.apply(left, right)

    @override
    def tostr(self) -> str:
        left = self.left.tostr()
        right = self.left.tostr()
        op = self.op.value
        return f"({left} {op} {right})"

if __name__ == "__main__":
    print()
    main()