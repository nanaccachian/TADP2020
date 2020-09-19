describe "TP" do
  describe '#pre' do
    class ClasePre
      pre { |num1, num2| num1 < 10 and num2 < 10 }
      def sumaMenor num1, num2
        num1 + num2
      end
    end

    it 'se cumple precondicion' do
      expect{ ClasePre.new.sumaMenor 5 , 5 }.not_to raise_error #('Error de invariante')
    end

    it 'no se cumple precondicion' do
      expect{ ClasePre.new.sumaMenor 11 , 5 }.to raise_error ('No se cumple la precondicion')
    end
  end

  describe '#post' do
    class ClasePost
      post { |result| result > 15 }
      def sumaMayor num1, num2
        num1 + num2
      end
    end

    it 'se cumple postcondicion' do
      expect{ ClasePost.new.sumaMayor 10 , 10 }.not_to raise_error #('Error de invariante')
    end

    it 'no se cumple postcondicion' do
      expect{ ClasePost.new.sumaMayor 9 , 5 }.to raise_error ('No se cumple la postcondicion')
    end
  end

  describe '#invariante' do

    class ClaseInv
      attr_accessor :variable
      invariant { variable < 10 }

      def initialize valorInicial
        @variable = valorInicial
      end
      def cambiarVariable nuevoValor
        @variable = nuevoValor
      end
    end

    it 'la inicializacion con valor 0<10 no produce excepción' do
      expect{ ClaseInv.new(0) }.not_to raise_error #('Error de invariante')
    end

    it 'la inicializacion con valor 15>10 produce excepción' do
      expect{ ClaseInv.new(15) }.to raise_error('Error de invariante')
    end

    it 'cambiarVariable con valor 5<10 no produce excepción' do
      expect{ ClaseInv.new(0).cambiarVariable(5) }.not_to raise_error #('Error de invariante')
    end

    it 'cambiarVariable con valor 100>10 produce excepción' do
      expect{ ClaseInv.new(0).cambiarVariable(100) }.to raise_error('Error de invariante')
    end
  end
end