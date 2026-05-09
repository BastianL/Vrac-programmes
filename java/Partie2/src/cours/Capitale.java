package cours;

public class Capitale extends Ville {
	private String monument;
	
	//Constructeur par défaut
	public Capitale() {
		//Ce mot clé appelle le constructeur de la classe mère
		super();
		monument = "aucun";
	}
	
	//Constructeur d'initialisation de la capitale
	public Capitale(String nom, int hab, String pays, String monument) throws NombreHabitantException, NomVilleException {
		super(nom, hab, pays);
		this.monument = monument;
	}
	
	/**
	 * Description d'une capitale
	 * @return String retourne la decription de l'objet
	 */
	public String decrisToi() {
		String str = super.decrisToi() + "\n \t ==>>" + this.monument+ " en est un monument";
		System.out.println("Invocation de super.decrisToi()");
		return str;
	}
/**
 * @return le nom du monument
 */
	public String getMonument() {
		return monument;
	}
	//Définit le nom du monument
	public void setMonument(String monument) {
		this.monument = monument;
	}
}
