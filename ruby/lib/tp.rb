class BeforeAndAfter
  @@before_list = []
  @@after_list = []

  def self.before_and_after_each_call(before, after)
    @@before_list.push(before)
    @@after_list.push(after)
  end

  def self.method_added mensaje
    unless @sobreescribiendo
      @sobreescribiendo = true
      method_clone = instance_method(mensaje).clone
      define_method mensaje do |*args|
        @@before_list.each { |before| before.call }
        _return = method_clone.bind(self).call *args
        @@after_list.each { |after| after.call }
        return _return
      end
      @sobreescribiendo = false
    end
  end
end

class Object
  @@invariantes = []
  @@precondicion = proc{ true }
  @@postcondicion = proc{ true }
  @@checkearInvariantes = true

  def self.invariant &invariante
    raise "No es una invariante" if invariante.arity != 0

    @@invariantes.push proc {
      @@checkearInvariantes = false
      valor = instance_eval &invariante
      @@checkearInvariantes = true
      valor
    }
  end

  def self.pre &precondicion
    @@precondicion = precondicion
  end

  def self.post &postcondicion
    @@postcondicion = postcondicion
  end

  def self.method_added mensaje
    unless @sobreescribiendo
      @sobreescribiendo = true
      metodoClon = instance_method(mensaje).clone
      precondicion = @@precondicion
      postcondicion = @@postcondicion
      @@precondicion = proc{ true }
      @@postcondicion = proc{ true }

      define_method mensaje do |*args|
        raise "No se cumple la precondicion" if not precondicion.call *args
        _return = metodoClon.bind(self).call *args
        raise "No se cumple la postcondicion" if not postcondicion.call _return
        if @@checkearInvariantes
          (raise "Error de invariante" if not @@invariantes.all? { |invariante| instance_eval &invariante })
        end
        return _return
      end
      @sobreescribiendo = false
    end
  end
end

class MiClase
  attr_accessor :variable

  def initialize
    @variable = 1
  end

  invariant { variable < 10 }

  def romperVariable
    @variable = 100
  end

  pre { |num1, num2| num1 < 10 and num2 < 10 }
  post { |result| result < 15 }
  def sumaMenor num1, num2
    num1 + num2
  end

  pre { |num1, num2| num1 > 10 and num2 > 10 }
  def sumaMayor num1, num2
    num1 + num2
  end
end