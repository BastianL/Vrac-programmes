package test;

public class MoteurEssence extends Moteur {
	
	public MoteurEssence() {
		super();
		this.type = TypeMoteur.ESSENCE;
	}

	public MoteurEssence(String cylindre, Double prix) {
		super(cylindre, prix);
		this.type = TypeMoteur.ESSENCE;
	}
	
	public MoteurEssence(TypeMoteur type, String cylindre, Double prix) {
		super(type, cylindre, prix);
	}

}
