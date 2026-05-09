package test;

public class MoteurHybride extends Moteur {
	
	public MoteurHybride() {
		super();
		this.type=TypeMoteur.HYBRIDE;
	}

	public MoteurHybride(String cylindre, Double prix, TypeMoteur type) {
		super(type, cylindre, prix);
	}
	
	public MoteurHybride(String cylindre, Double prix) {
		super(cylindre, prix);
		this.type = TypeMoteur.HYBRIDE;
	}

}
