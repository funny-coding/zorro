package xyz.funnycoding.days

object intMachine {
  type IntMachine         = Map[Int, Int]
  type ArithmeticFunction = Int => Int => Int
  type Noun               = Int
  type Verb               = Int

  sealed trait OpCode
  case class Add(p1: Parameter, p2: Parameter, p3: Parameter) extends OpCode
  case class Multiply(p1: Parameter, p2: Parameter, p3: Parameter) extends OpCode
  case class Input(p1: Parameter) extends OpCode
  case class Output(p1: Parameter) extends OpCode
  case object Halt extends OpCode
  case object Error extends OpCode

  sealed trait Parameter
  case object Position extends Parameter
  case object Immediate extends Parameter

  case class MachineCombination(noun: Noun, verb: Verb, output: Int) {
    def result = 100 * noun + verb
  }

  def buildParameter(`type`: Int): Parameter = `type` match {
    case 0 => Position
    case 1 => Immediate
  }

  object OpCode {
    def opCode(int: Int): OpCode = {
      def extend(instruction: List[Int]): List[Int] = instruction match {
        case i if i.endsWith(List(9, 9)) => List(9, 9)
        case i if i.endsWith(List(1))    => leftPad(i)(0, 5)
        case i if i.endsWith(List(2))    => leftPad(i)(0, 5)
        case i if i.endsWith(List(3))    => leftPad(i)(0, 3)
        case i if i.endsWith(List(4))    => leftPad(i)(0, 3)
        case _                           => instruction
      }

      def leftPad[A](source: List[A])(pad: A, size: Int): List[A] =
        List.fill(size - source.size)(pad) ++ source

      extend(int.toString().toCharArray().map(_.asDigit).toList) match {
        case List(9, 9) => Halt
        case List(0, p2, p1, 0, 1) =>
          Add(
            buildParameter(p1),
            buildParameter(p2),
            Position
          )
        case List(0, p2, p1, 0, 2) =>
          Multiply(buildParameter(p1), buildParameter(p2), Position)

        case List(0, 0, 3) =>
          Input(Position)

        case List(p1, 0, 4) => Output(buildParameter(p1))
        case _              => Error

      }
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
    def run(machine: IntMachine): Option[(IntMachine, List[Int])] = {
      def go(instructionPointer: Int, machine: IntMachine, outputs: List[Int]): Option[(IntMachine, List[Int])] =
        opCode(valueAtIndex(instructionPointer, machine)) match {
          case a: Add => {
            val op = operation(instructionPointer, machine, outputs, a)
            go(instructionPointer + 4, op._1, op._2)
          }
          case m: Multiply => {
            val op = operation(instructionPointer, machine, outputs, m)
            go(instructionPointer + 4, op._1, op._2)
          }
          case i: Input => {
            val op = operation(instructionPointer, machine, outputs, i)
            go(instructionPointer + 2, op._1, op._2)
          }
          case o: Output => {
            val op = operation(instructionPointer, machine, outputs, o)
            go(instructionPointer + 2, op._1, op._2)
          }
          case Halt => Some((machine, outputs))
          case Error =>
            None

        }
      go(0, machine, Nil)
    }

    def evaluate(intMachine: IntMachine): Int = run(intMachine).map(_._1).map(_.getOrElse(0, 0)).getOrElse(0)

    def outputs(intMachine: IntMachine): List[Int] = run(intMachine).map(_._2).getOrElse(Nil)

    def resolve(p: Parameter, index: Int)(machine: IntMachine) = p match {
      case Immediate => {
        machine.getOrElse(index, 0)
      }
      case Position => {
        val position1 = valueAtIndex(index, machine)
        valueAtIndex(position1, machine)
      }
    }

    def operation(
        instructionPointer: Int,
        machine: IntMachine,
        outputs: List[Int],
        opCode: OpCode
    ): (IntMachine, List[Int]) =
      opCode match {
        case Add(p1, p2, _) => {
          val v1 = resolve(p1, instructionPointer + 1)(machine)
          val v2 = resolve(p2, instructionPointer + 2)(machine)
          val v3 = valueAtIndex(instructionPointer + 3, machine)
          (machine.updated(v3, v1 + v2), outputs)
        }
        case Multiply(p1, p2, _) => {

          val v1 = resolve(p1, instructionPointer + 1)(machine)
          val v2 = resolve(p2, instructionPointer + 2)(machine)
          val v3 = valueAtIndex(instructionPointer + 3, machine)
          (machine.updated(v3, v1 * v2), outputs)
        }
        case Input(_) => {
          val v = machine.getOrElse(instructionPointer + 1, 0)
          (machine.updated(v, 1), outputs) // hard coded input
        }
        case Output(p1) => {
          val v1 = resolve(p1, instructionPointer + 1)(machine)
          (machine, outputs :+ v1)
        }
        case _ => (machine, outputs)
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

    def nounAndVerb(machine: IntMachine): Int =
      iterate(machine)
        .filter(_.output == outputToProduce)
        .headOption
        .map(_.result)
        .getOrElse(0)

  }

}
