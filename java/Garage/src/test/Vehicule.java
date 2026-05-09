package test;

import java.util.List;

public class Vehicule {
	
	protected double prix;
	protected String nom;
	private List<Option> options;
	protected Marque nomMarque;
	private Moteur Moteur;
	
	public Vehicule() {
		this.prix = 0;
		this.nom = null;
		this.options = null;
		this.nomMarque = null;
	}
	
	public Vehicule(double pPrix, String pNom, List<Option> pOptions, Marque pMarque) {
		this.prix = pPrix;
		this.nom = pNom;
		this.options = pOptions;
		this.nomMarque = pMarque;
	}
	
	public String toString() {
		return "Voiture" + nomMarque + ":" ;
	}

	public void addOption(Option opt) {
		options.add(opt);
	}
	
	public Marque getMarque() {
		return nomMarque;
	}
	
	public List<Option> getOptions(){
		return options;
	}
	
	public Double getPrix() {
		return prix;
	}

	public void setMoteur(Moteur obj) {
		this.Moteur = obj;
	}

	
}
