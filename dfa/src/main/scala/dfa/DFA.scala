package dfa // leave this line in the file

// TODO: replace this comment with your implementation

case class State(label: String):

  def label: String = label
//   def minutes: Int = (amount / 60) % 60
//   def hours:   Int = (amount / 3600).toInt

//   override def toString = f"$hours%02d:$minutes%02d:$seconds%02d"

//   def +(other: Stopwatch) = Stopwatch(amount + other.amount)
//   def +(moreSeconds: Int) = Stopwatch(amount + moreSeconds)

case class Transition():
    def from: State = 
    def symbol: Char = 
    def to: State = 

class DFA():
    def states: Set[State] = 
    def transitions: Set[Transition] = 
    def startState: State = 
    def acceptingStates: Set[State] = 
        