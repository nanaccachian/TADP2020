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

class MiClase
  @@invariantes = []
  @@precondicion = proc{ true }
  @@postcondicion = proc{ true }

  def self.invariant &invariante
    raise "No es una invariante" if invariante.arity != 0
    @@invariantes.push invariante
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
        unless @checkearInvariantes
          @checkearInvariantes = true
          unless (@@invariantes.all? proc { |invariante| instance_eval &invariante})
            @checkearInvariantes = false
            raise "Error de invariante"
          end
          @checkearInvariantes = false
        end
        return _return
      end
      @sobreescribiendo = false
    end
  end
end