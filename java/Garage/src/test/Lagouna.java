package test;

import java.util.List;

public class Lagouna extends Vehicule {

	public Lagouna(){
		super();
		this.nom = "Lagouna";
		this.prix = 23123.0;
		this.nomMarque = Marque.RENO;
	}
	
	public Lagouna(Double prix, String nom, List<Option> options, Marque nomMarque) {
		super(prix, nom, options, nomMarque);
	}
	
}
