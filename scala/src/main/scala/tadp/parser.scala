package tadp

import scala.annotation.tailrec
import scala.util._

trait Parser[+A] extends (String => Try[ParserResult[A]]) {

  def <|>[U >: A](parserB: => Parser[U]): Parser[U] = input =>
    this (input).orElse(parserB(input))

  //Try(this (input).getOrElse(parserB(input).get))
  /*
    Para el type parameter, podrían pensarlo así tambien (versión larga):
    class Parser[+T]
        def <|>[U >: T, V >: U](parserB: Parser[U]): Parser[V]
    Aca estamos diciendo:
      - tengo en la mano un Parser de T
      - recibo por parámetro un parser de U
      - la respuesta es un parser de tipo V
      - la relación entre U, T y V es: "T extiende de U que extiende de V" => T -> U -> V
      - entonces, el compilador elige el V más cercano para que la herencia de ambos se cumpla 
    
    
    Por otro lado, hay una implementación más común y compacta para hacer esto mismo, les copio la 
    definición del "orElse" que tiene Try como ejemplo y de ahí extrapolo para el <|>
      
      sealed abstract class Try[+T] extends Product with Serializable {
        ...
        def orElse[U >: T](default: => Try[U]): Try[U]
        
    Ahí pueden ver que "T" es el tipo del try, cuando hacen "orElse" recibe una función / parametro lazy que retorna
    un Try[U], donde U es un SUPER tipo de T (y ese super es muy interesante).
    
    El compilador en este caso elige un U tal que T y U compartan una linea de herencia.
    Lo interesante es que U **no tiene que ser especificamente** el tipo del parametro que le pasaron, sino que
    puede ser cualquiera de sus super tipos (una versión más general del tipo de Try[U] especifica que le paso).
    
    Para que esto funcione bien, tenemos que especificar la relación de herencia de mis "Parser[A]", es decir, 
    la varianza que van a tener mis parámetros de tipo (sino son invariantes y nadie extiende de nadie si el A es diferente).
    Para esto, A es CO-variante (+), igual que en el Try (significa que Parser[A] sigue la misma relación de "herencia" / "padre-hijo" que A).
    
    Por ejemplo:
   */
  def asdf() = {
    trait Animal
    class Persona extends Animal
    class Perro extends Animal
    val persona: Try[Persona] = ???
    val perro: Try[Perro] = ???

    val resultInferido = persona.orElse(perro)
    val resultExplicito: Try[Animal] = persona.orElse(perro)

    // --- parser
    // Ahora puedo escribir un parser <|> con herencia!!!
    val parserPersona: Parser[Persona] = ???
    val parserPerro: Parser[Perro] = ???
    val resultParserInferido = parserPersona <|> parserPerro
    val resultParserExplicito: Parser[Animal] = parserPersona <|> parserPerro
  }

  /*
    Ahí pueden ver como el compilador solito decidió que la parte común entre Persona y Perro es el tipo Animal.
    
    TODOS los tipos tienen un padre común (la jerarquía de tipos de scala tiene un padre común (Any) y un hijo común (Nothing).
    Es decir, en el peor de los casos, el tipo común entre ambos es "Any", por ejemplo:
   */
  def asdf2() = {
    val string: Try[String] = ???
    val int: Try[Int] = ???
    val resultInferido = string.orElse(int)
    val resultExplicito: Try[Any] = string.orElse(int)
  }

  // TODO saquen este Try {} y los demás que usen por una mecanica similar a la anterior
  def <>[B](parserB: => Parser[B]): Parser[(A, B)] = input => Try {
    this.andThen(resultA => {
      val resultB = parserB(resultA.get.output).get
      ParserResult((resultA.get.consumed, resultB.consumed), resultB.output)
    })(input)
  }

  def ~>[B](parserB: => Parser[B]): Parser[B] = this (_).flatMap(r => parserB(r.output))

  def <~[B](parserB: => Parser[B]): Parser[A] = this (_).flatMap(a => parserB(a.output).map(b => a.copy(output = b.output)))

  def sepBy[B](parserB: => Parser[B]): Parser[List[A]] = (this <> (parserB ~> this).*).map { case (first, list) => first :: list }

  def satisfies(condition: => A => Boolean): Parser[A] = this (_).flatMap {
    case result if condition(result.consumed) => Success(result)
    case _ => Failure(new ParserError)
  }

  def opt(): Parser[Option[A]] = input =>
    // esto es una forma más compacta, evitando el pattern match
    // TODO esto sería más feliz si ParserResult entendiera "map"
    // TODO otra es modelar ParserResult como se modela Option o Try (como un trait con dos hijos)
    //   Entonces, podrían tener un ParserResultSuccess y un ParserResultFailure, 
    //   donde sucess es igual al ParserResult actual y ParserResultFailure tiene como parametros lo que se consumió, el error y alguna otra cosa que necesiten.
    //   Si hacen eso, pueden definir un "orElse", "map", "flatMap" a nivel de ParserResult y esta lógica que se repite seguido (abrir el Try y hacer algo)
    //   quedaría generalizada.
    //   MORALEJA: armen esa herencia como mejora en la implementación
    this (input)
      .map(r => ParserResult(Some(r.consumed), r.output))
      .orElse(Success(ParserResult(None, input)))

  //  def opt(): Parser[Option[A]] = input => this (input) match {
  //    case Success(ParserResult(consumed, output)) => Success(ParserResult(Option(consumed), output))
  //    case Failure(_) => Success(ParserResult(Option.empty, input))
  //  }

  def * : Parser[List[A]] = input => {
    // Versión inmutable al algoritmo que tenían antes:
    @tailrec
    def recursiveKleene(output: String, consumed: List[A]): ParserResult[List[A]] = {
      this (output) match {
        case Success(ParserResult(parsed, parsedOutput)) => recursiveKleene(parsedOutput, consumed :+ parsed)
        case Failure(_) => ParserResult(consumed, output)
      }
    }

    Success(recursiveKleene(input, List()))

    //    var consumed = List[A]()
    //    var output = input
    //
    //    var result = this (input)
    //    while (result.isSuccess) {
    //      consumed = consumed :+ result.get.consumed
    //      output = result.get.output
    //      result = this (output)
    //    }
    //
    //    Success(ParserResult(consumed, output))
  }

  def + : Parser[List[A]] = (this <> this.*).map { case (first, list) => first :: list }

  def map[B](transform: => A => B): Parser[B] = this (_).map(result => result.copy(consumed = transform(result.consumed)))
}

case class ParserResult[+T](consumed: T, output: String)

class ParserError extends RuntimeException("Parser error")
