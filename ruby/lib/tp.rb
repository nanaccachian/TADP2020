module Contracts
  def method_add_before before
    @precontratos ||= []
    @precontratos << before
  end

  def method_add_after after
    @poscontratos ||= []
    @poscontratos << after
  end

  def method_added mensaje
    unless @sobreescribiendo
      @sobreescribiendo = true

      @precontratos ||= []
      @poscontratos ||= []
      before_list = @precontratos.clone
      after_list = @poscontratos.clone

      method = instance_method(mensaje)
      define_method mensaje do |*args, &block|
        before_list.each { |before| instance_exec(*args, &before) }
        _return = method.bind(self).call *args, &block
        after_list.each { |after| instance_exec(_return, &after) }
        return _return
      end

      @precontratos = []
      @poscontratos = []
    end
  ensure
    @sobreescribiendo = false
  end

  def checkearInvariantes(instancia)
  end
end

module BeforeAndAfter
  def self.included(klass)
    super klass
    klass.extend(Contracts)
    klass.extend(BeforeAndAfterMethods)
  end

  module BeforeAndAfterMethods
    def before_and_after_each_call(before, after)
      @before ||= []
      @after ||= []
      @before << before
      @after << after
    end

    def chequearBefore instancia
      unless @checkeandoByA
        @checkeandoByA = true
        @before ||= []
        @before.each { |metodo| instancia.instance_eval &metodo }
        @checkeandoByA = false
      end
    end

    def chequearAfter instancia
      unless @checkeandoByA
        @checkeandoByA = true
        @after ||= []
        @after.each { |metodo| instancia.instance_eval &metodo }
        @checkeandoByA = false
      end
    end

    def method_added mensaje
      method_add_before Proc.new { self.class.chequearBefore self }
      method_add_after Proc.new { self.class.chequearAfter self }
      super mensaje
    end
  end
end

module PreAndPost
  def self.included(klass)
    super klass
    klass.extend(Contracts)
    klass.extend(PreAndPostMethods)
  end

  module PreAndPostMethods
    def pre &precondicion
      @precondicion = precondicion
    end

    def post &postcondicion
      @postcondicion = postcondicion
    end

    def method_added mensaje
      method = instance_method(mensaje)

      if @precondicion
        precondicion = @precondicion
        method_add_before Proc.new { |*args|
          params = method.parameters.map{ |param| param[1] }.filter{ |param| param }
          if not method.parameters.empty?
            instance_eval do
              raise "No se cumple la precondicion de #{mensaje.to_s}" if not Struct.new(*params).new(*args).instance_eval &precondicion
            end
          else
            raise "No se cumple la precondicion de #{mensaje.to_s}" if not instance_eval &precondicion
          end
        }
        @precondicion = nil
      end

      if @postcondicion
        postcondicion = @postcondicion
        method_add_after Proc.new { |resultado|
          raise "No se cumple la postcondicion de #{mensaje.to_s}" if not postcondicion.call resultado
        }
        @postcondicion = nil
      end

      super mensaje
    end
  end
end

module Invariants
  def self.included(klass)
    super klass
    klass.extend(Contracts)
    klass.extend(InvariantsMethods)
  end

  module InvariantsMethods
    def invariant &invariante
      @invariantes ||= []
      @invariantes << invariante
    end

    def checkearInvariantes(instancia)
      if @invariantes and not @ignorarInvariantes
        @ignorarInvariantes = true
        @checkeandoByA = true
        unless (@invariantes.all? proc { |invariante| instancia.instance_eval &invariante})
          @ignorarInvariantes = false
          @checkeandoByA = false
          raise "Error de invariante"
        end
        @ignorarInvariantes = false
        @checkeandoByA = false
      end
    end

    def method_added mensaje
        method_add_after Proc.new {
          self.class.checkearInvariantes(self)
        }
        super mensaje
    end
  end
end