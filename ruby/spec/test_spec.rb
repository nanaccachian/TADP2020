describe MiClase do
  describe '#pre' do
    class MiClase
      def initialize #NO TIENE SENTIDO :)
        @variable = 0
      end

      pre { |num1, num2| num1 < 10 and num2 < 10 }
      def sumaMenor num1, num2
        num1 + num2
      end
    end

    it 'se cumple precondicion' do
      expect{ MiClase.new.sumaMenor 5 , 5 }.not_to raise_error #('Error de invariante')
    end

    it 'no se cumple precondicion' do
      expect{ MiClase.new.sumaMenor 11 , 5 }.to raise_error ('No se cumple la precondicion')
    end
  end

  describe '#post' do
    class MiClase

      post { |result| result > 15 }
      def sumaMayor num1, num2
        num1 + num2
      end
    end

    it 'se cumple postcondicion' do
      expect{ MiClase.new.sumaMayor 10 , 10 }.not_to raise_error #('Error de invariante')
    end

    it 'no se cumple postcondicion' do
      expect{ MiClase.new.sumaMayor 9 , 5 }.to raise_error ('No se cumple la postcondicion')
    end
  end

  describe '#invariante' do

    class MiClase
      attr_accessor :variable
      invariant { variable < 10 }
    end

    it 'la inicializacion no produce excepción' do
      class MiClase
        def initialize
          @variable = 0
        end
      end
      expect{ MiClase.new }.not_to raise_error #('Error de invariante')
    end

    it 'cambiarVariable no produce excepción' do
      class MiClase
        def cambiarVariable
          @variable = 5
        end
      end
      expect{ MiClase.new.cambiarVariable }.not_to raise_error #('Error de invariante')
    end

    it 'romperVariable produce excepción' do
      class MiClase
        def romperVariable
          @variable = 100
        end
      end
      expect{ MiClase.new.romperVariable }.to raise_error('Error de invariante')
    end

    it 'la inicializacion produce excepción' do
      class MiClase
        def initialize
          @variable = 15
        end
      end
      expect{ MiClase.new }.to raise_error('Error de invariante')
    end
  end
end