package test;

public class MoteurDiesel extends Moteur {
	
	public MoteurDiesel() {
		super();
		this.type = TypeMoteur.DIESEL;
	}
	
	public MoteurDiesel(String cylindre, Double prix) {
		super(cylindre, prix);
		this.type = TypeMoteur.DIESEL;
	}
	
	public MoteurDiesel(String cylindre, Double prix, TypeMoteur Type) {
		super(Type, cylindre, prix);
	}

}
