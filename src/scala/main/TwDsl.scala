package TwDsl

trait TwDsl {
    // Abstract Type
    type E

    // Operations on abstract type
    def int(i: Int): E
    def infix_+
}