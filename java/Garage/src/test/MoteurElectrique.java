package test;

public class MoteurElectrique extends Moteur {
	
	public MoteurElectrique() {
		super();
		this.type = TypeMoteur.ELECTRIQUE;
	}
	
	public MoteurElectrique(String cylindre, Double prix, TypeMoteur type) {
		super(type, cylindre, prix);
	}

	public MoteurElectrique(String cylindre, Double prix){
		super(cylindre, prix);
		this.type = TypeMoteur.ELECTRIQUE;
	}

}
