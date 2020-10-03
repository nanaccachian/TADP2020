describe "TP" do
  describe '#pre' do
    testClass = Class.new do
      include PreAndPost
      attr_accessor :precondicionOk

      pre { precondicionOk }
      def testPrecondicion
      end

      pre { numA<10 and numB<10 }
      def sumaMenor numA, numB
        numA + numB
      end
    end

    it 'Se cumple precondicion 5<10' do
      expect { testClass.new.sumaMenor 5 , 5 }.not_to raise_error
    end

    it 'No se cumple precondicion 11>10' do
      expect { testClass.new.sumaMenor 11 , 5 }.to raise_error ('No se cumple la precondicion de sumaMenor')
    end

    it 'La precondicion entiende el contexto de la clase y pasa' do
      testInstance = testClass.new
      testInstance.precondicionOk =true
      expect { testInstance.testPrecondicion }.not_to raise_error
    end

    it 'La precondicion entiende el contexto de la clase y no pasa' do
      testInstance = testClass.new
      testInstance.precondicionOk = false
      expect { testInstance.testPrecondicion }.to raise_error ('No se cumple la precondicion de testPrecondicion')
    end

    it 'Las precondiciones no se superponen, ambas sumas cumplen su precondicion' do
      testClass2 = Class.new do
        include PreAndPost

        pre { numA>20 and numB>20 }
        def sumaMayor numA, numB
          numA + numB
        end

        pre { numA<10 and numB<10 }
        def sumaMenor numA, numB
          numA + numB
        end
      end

      expect { testClass2.new.sumaMenor 1,3 }.not_to raise_error
      expect { testClass2.new.sumaMayor 21,53 }.not_to raise_error
    end
  end

  describe '#post' do
    testClass = Class.new do
      include PreAndPost
      post { |result| result>50 }
      def sumaMayor numA, numB
        numA + numB
      end
    end

    it 'Se cumple postcondicion 95>50' do
      expect { testClass.new.sumaMayor 35, 60 }.not_to raise_error
    end

    it 'No se cumple postcondicion 14<50' do
      expect{ testClass.new.sumaMayor 9, 5 }.to raise_error ('No se cumple la postcondicion de sumaMayor')
    end

    it 'Las postcondiciones no se superponen, ambas sumas cumplen su precondicion' do
      testClass2 = Class.new do
        include PreAndPost
        post { |result| result>50 }
        def sumaMayor numA, numB
          numA + numB
        end

        post { |result| result<20 }
        def sumaMenor numA, numB
          numA + numB
        end
      end

      expect{ testClass2.new.sumaMenor 1 , 3 }.not_to raise_error
      expect{ testClass2.new.sumaMayor 35 , 60 }.not_to raise_error
    end
  end

  describe '#invariante' do
    testClass = Class.new do
      include Invariants
      attr_accessor :variable

      invariant { variable < 10 }

      def initialize variable
        @variable = variable
      end

      def cambiarVariable variable
        @variable = variable
      end
    end

    it 'La inicializacion con valor 0<10 no produce excepción' do
      expect { testClass.new(0) }.not_to raise_error
    end

    it 'La inicializacion con valor 15>10 produce excepción' do
      expect { testClass.new(15) }.to raise_error('Error de invariante')
    end

    it 'CambiarVariable con valor 5<10 no produce excepción' do
      expect { testClass.new(0).cambiarVariable(5) }.not_to raise_error
    end

    it 'CambiarVariable con valor 100>10 produce excepción' do
      expect { testClass.new(0).cambiarVariable(100) }.to raise_error('Error de invariante')
    end

    it 'Variable= con valor 5<10 no produce excepción' do
      expect { testClass.new(0).variable = 5 }.not_to raise_error
    end

    it 'Variable= con valor 100>10 produce excepción' do
      expect{ testClass.new(0).variable = 100 }.to raise_error('Error de invariante')
    end
    childClass = Class.new(testClass) do end

    xit 'La inicializacion de una clase hijo' do
      expect { childClass.new(15) }.to raise_error('Error de invariante')
    end
  end

  describe '#BeforeAndAfter' do
    it 'El before se llama antes del mensaje' do
      testClass = Class.new do
        include BeforeAndAfter
        attr_accessor :lista

        before_and_after_each_call(proc { agregar 1 }, proc { })

        def agregar num
          @lista ||= []
          @lista << num
        end
      end

      testInstance =  testClass.new
      testInstance.agregar 2
      expect(testInstance.lista.to_a).to match_array([1, 2])
    end

    it 'El after se llama despues del mensaje' do
      testClass = Class.new do
        include BeforeAndAfter
        attr_accessor :lista

        before_and_after_each_call( proc{ }, proc{ agregar 2 })

        def agregar num
          @lista ||= []
          @lista << num
        end
      end

      testInstance =  testClass.new
      testInstance.agregar 1
      expect(testInstance.lista.to_a).to match_array([1,2])
    end

    it 'El before y after funcionan en conjunto' do
      testClass = Class.new do
        include BeforeAndAfter
        attr_accessor :lista

        before_and_after_each_call( proc { agregar 1 }, proc { agregar 3 })

        def agregar num
          @lista ||= []
          @lista << num
        end
      end

      testInstance = testClass.new
      testInstance.agregar 2
      expect(testInstance.lista.to_a).to match_array([1, 2, 3])
    end

    it 'si hay mas de un before_and_after, se ejecutan todos en orden' do
      testClass = Class.new do
        include BeforeAndAfter
        attr_accessor :lista

        before_and_after_each_call( proc { agregar 1 }, proc { agregar 5 })
        before_and_after_each_call( proc { agregar 2 }, proc { agregar 6 })
        before_and_after_each_call( proc { agregar 3 }, proc { agregar 7 })

        def agregar num
          @lista ||= []
          @lista << num
        end
      end

      testInstance = testClass.new
      testInstance.agregar 4
      expect(testInstance.lista.to_a).to match_array([1, 2, 3, 4, 5, 6, 7])
    end
  end

  describe '#CrossOvers' do
    testClass = Class.new do
      include Invariants
      include PreAndPost
      include BeforeAndAfter
      attr_accessor :variable

      invariant { variable < 10 }
      before_and_after_each_call( proc { variable = 5 }, proc { })

      def initialize
        @variable = 0
      end

      def cambiarVariable variable
        @variable = variable
      end

      post { |result| result>50 }
      def sumaMayor numA, numB
        numA + numB
      end

      pre { numA<10 and numB<10 }
      def sumaMenor numA, numB
        numA + numB
      end
    end

    xit 'La invariante rompe y la postcondicion no' do
      expect { testClass.new.sumaMayor 10, 30 }.to raise_error('No se cumple la postcondicion de sumaMayor')
      expect { testClass.new.cambiarVariable 20 }.to raise_error('Error de invariante')
    end

    xit 'La precondicion rompe y before pasa' do
      testInstance = testClass.new
      expect { testInstance.sumaMenor 10, 30 }.to raise_error('No se cumple la precondicion de sumaMenor')
      expect(testInstance.variable).to eq(5)
    end
  end
end