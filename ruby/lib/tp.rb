module BeforeAndAfter
  def self.included(klass)
    super klass
    klass.extend(BeforeAndAfterMethods)
  end

  module BeforeAndAfterMethods
    def before_and_after_each_call(before, after)
      @before_list ||= []
      @after_list ||= []
      @before_list << before
      @after_list << after
    end

    def method_added mensaje
      unless @sobreescribiendo
        @sobreescribiendo = true

        @before_list ||= []
        @after_list ||= []
        before_list = @before_list
        after_list = @after_list

        method = instance_method(mensaje)
        define_method mensaje do |*args, &block|
          before_list.each { |before| instance_eval &before } if before_list
          _return = method.bind(self).call *args, &block
          after_list.each { |after| instance_eval &after } if after_list
          return _return
        end

        #@sobreescribiendo = false
        super mensaje
      end
    end
  ensure
    @sobreescribiendo = false
  end
end

module PreAndPost
  def self.included(klass)
    super klass
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
      unless @sobreescribiendo
        @sobreescribiendo = true

        precondicion = @precondicion || proc{ true }
        postcondicion = @postcondicion || proc{ true }
        @precondicion = nil
        @postcondicion = nil

        method = instance_method(mensaje)

        define_method mensaje do |*args, &block|
          params = method.parameters.map{ |param| param[1] }.filter{ |param| param }
          if params.length == args.length and not method.parameters.empty?
            instance_eval do
              raise "No se cumple la precondicion de #{mensaje.to_s}" if not Struct.new(*params).new(*args).instance_eval &precondicion
            end
          else
            raise "No se cumple la precondicion de #{mensaje.to_s}" if not instance_eval &precondicion
          end
          _return = method.bind(self).call *args, &block
          raise "No se cumple la postcondicion de #{mensaje.to_s}" if not postcondicion.call _return
          return _return
        end

        #@sobreescribiendo = false
        super mensaje
      end
    ensure
      @sobreescribiendo = false
    end
  end
end

module Invariants
  def self.included(klass)
    super klass
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
        unless (@invariantes.all? proc { |invariante| instancia.instance_eval &invariante})
          @ignorarInvariantes = false
          raise "Error de invariante"
        end
      end
    ensure
      @ignorarInvariantes = false
    end

    def method_added mensaje
      method = instance_method(mensaje)
      unless @sobreescribiendo
        @sobreescribiendo = true

        define_method mensaje do |*args, &block|
          _return = method.bind(self).call *args, &block
          self.class.checkearInvariantes(self)
          return _return
        end

        #@sobreescribiendo = false
        super mensaje
      end
    ensure
      @sobreescribiendo = false
    end
  end
end