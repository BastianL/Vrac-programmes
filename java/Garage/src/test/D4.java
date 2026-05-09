package test;

import java.util.List;

public class D4 extends Vehicule {

	public D4() {
		super();
		this.nom = "D4";
		this.prix = 25147.0;
		this.nomMarque = Marque.TROEN;
	}
	
	public D4(Double prix, String nom, List<Option> options, Marque nomMarque) {
		super(prix, nom, options, nomMarque);
	}
	
}
