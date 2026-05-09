package test;

import java.util.List;

public class A300B extends Vehicule {
	
	public A300B(){
		super();
		this.nom = "A300B";
		this.prix = 28457.0;
		this.nomMarque = Marque.PIGEOT;
	}
	
	public A300B(Double prix, String nom, List<Option> options, Marque nomMarque) {
		super(prix, nom, options, nomMarque);
	}
	
}
