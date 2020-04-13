package xyz.funnycoding.days

object intMachine {
  type IntMachine         = Map[Int, Int]
  type ArithmeticFunction = Int => Int => Int
  type Noun               = Int
  type Verb               = Int

  sealed trait OpCode
  case object Add extends OpCode
  case object Multiply extends OpCode
  case object Halt extends OpCode
  case object Error extends OpCode

  case class MachineCombination(noun: Noun, verb: Verb, output: Int) {
    def result = 100 * noun + verb
  }

  object OpCode {
    def opCode(int: Int): OpCode = int match {
      case 1  => Add
      case 2  => Multiply
      case 99 => Halt
      case _  => Error
    }
  }

  object Parser {
    def parse(str: String): IntMachine = {
      // not safe
      val intList = str.split(',').map(_.toInt)
      Range.inclusive(0, intList.size).zip(intList).toMap
    } //.updated(2, 2).updated(1, 12)
  }

  object Runner {
    import OpCode._
    val outputToProduce: Int = 19690720
    def run(machine: IntMachine): Option[IntMachine] = {
      def go(instructionPointer: Int, machine: IntMachine): Option[IntMachine] =
        opCode(valueAtIndex(instructionPointer, machine)) match {
          case Add =>
            go(instructionPointer + 4, operation(instructionPointer, machine, ((a: Int, b: Int) => a + b).curried))
          case Multiply =>
            go(instructionPointer + 4, operation(instructionPointer, machine, ((a: Int, b: Int) => a * b).curried))
          case Halt  => Some(machine)
          case Error => None
        }
      go(0, machine)
    }

    def evaluate(intMachine: IntMachine): Int = run(intMachine).map(_.getOrElse(0, 0)).getOrElse(0)

    def operation(instructionPointer: Int, machine: IntMachine, func: ArithmeticFunction): IntMachine = {
      val position1        = valueAtIndex(instructionPointer + 1, machine)
      val position2        = valueAtIndex(instructionPointer + 2, machine)
      val position3        = valueAtIndex(instructionPointer + 3, machine)
      val valueAtPosition1 = valueAtIndex(position1, machine)
      val valueAtPosition2 = valueAtIndex(position2, machine)
      machine.updated(position3, func(valueAtPosition1)(valueAtPosition2))
    }

    def valueAtIndex(index: Int, machine: IntMachine): Int =
      machine.getOrElse(index, 0)

    private def range(from: Int, to: Int): List[Int] = Range.inclusive(from, to).toList

    def iterate(machine: IntMachine): List[MachineCombination] =
      for {
        noun <- range(0, 99)
        verb <- range(0, 99)
        updated = machine.updated(1, noun).updated(2, verb)
      } yield MachineCombination(noun, verb, evaluate(updated))

    def nounAndVerb(machine: IntMachine): Int = iterate(machine).filter(_.output == outputToProduce).head.result

  }

}
