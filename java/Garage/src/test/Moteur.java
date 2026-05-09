package test;

public abstract class Moteur {
	protected TypeMoteur type;
	protected String cylindre;
	protected Double prix;
	
	public Moteur () {
		this.type = null;
		this.cylindre = null;
		this.prix = null;
	}
	
	public Moteur (TypeMoteur ptype, String pcylindre, Double pprix) {
		this.type = ptype;
		this.cylindre = pcylindre;
		this.prix = pprix;
	}
	
	public Moteur(String cylindre, Double prix) {
		this.cylindre = cylindre;
		this.prix = prix;
	}
	
	public String toString(){
		return null;
	}
	
	public Double getPrix() {
		return prix;
	}
	
}
