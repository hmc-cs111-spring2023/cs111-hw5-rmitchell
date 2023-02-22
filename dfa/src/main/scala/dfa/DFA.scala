package dfa // leave this line in the file

// TODO: replace this comment with your implementation

case class State (label:String)
case class Transition (from:State, symbol:Char, to:State)

class DFA(states:Set[State], transitions:Set[Transition],
startState: State, acceptingStates:Set[State]):
    def accepts(input:String): Boolean = {
    input match {
    case "" => True
    case _ => 
  }
}
