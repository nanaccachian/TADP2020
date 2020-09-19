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
  def self.pre &precondicion
    @precondicion = precondicion
  end

  def self.post &postcondicion
    @postcondicion = postcondicion
  end

  def self.invariant &invariante
    raise "No es una invariante" if invariante.arity != 0
    if @invariantes != nil
      @invariantes.push invariante
    else
      @invariantes = [invariante]
    end
  end

  def self.checkearInvariantes(instancia)
    if not @ignorarInvariantes and @invariantes != nil
      @ignorarInvariantes = true
      unless (@invariantes.all? proc { |invariante| instancia.instance_eval &invariante})
        @ignorarInvariantes = false
        raise "Error de invariante"
      end
      @ignorarInvariantes = false
    end
  end

  def self.method_added mensaje
    unless @sobreescribiendo
      @sobreescribiendo = true
      metodoClon = instance_method(mensaje).clone
      precondicion = @precondicion != nil ? @precondicion : proc{ true }
      postcondicion = @postcondicion != nil ? @postcondicion : proc{ true }
      @precondicion = proc{ true }
      @postcondicion = proc{ true }

      define_method mensaje do |*args|
        raise "No se cumple la precondicion" if not precondicion.call *args
        _return = metodoClon.bind(self).call *args
        raise "No se cumple la postcondicion" if not postcondicion.call _return
        self.class.checkearInvariantes(self)
        return _return
      end
      @sobreescribiendo = false
    end
  end
end
