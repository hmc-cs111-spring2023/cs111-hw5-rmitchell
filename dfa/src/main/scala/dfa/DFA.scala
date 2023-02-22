package dfa // leave this line in the file

// TODO: replace this comment with your implementation

case class State (label:String)
case class Transition (from:State, to:State, symbol:Char)

class DFA(states:Set[State], val transitions:Set[Transition],
val start: State, val accept:Set[State]):
    
  def accepts(input:String): Boolean = {
    input match {
    case "" => 
      if (accept.contains(this.start)){return true} 
      else {return false}
    case stringInput:Any =>
      val newList = this.transitions.filter(_.from == this.start).filter(_.symbol == stringInput.charAt(0))
      val newStartState:State = (newList.head).to	
      val recursiveDFA:DFA = DFA(this.states, this.transitions, newStartState, this.accept)
      return recursiveDFA.accepts(stringInput.substring(1))
      
  }
}