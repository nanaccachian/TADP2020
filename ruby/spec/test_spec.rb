describe MiClase do
  describe '#invariante' do
    it 'RomperVariable produce excepción' do
      expect{ MiClase.new.romperVariable }.to raise_error('Error de invariante')
    end
  end
end