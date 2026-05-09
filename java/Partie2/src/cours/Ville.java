package cours;

public class Ville {
	public static int nbreInstances = 0;
	protected static int nbreInstancesBis = 0;
	protected String nomVille;
	protected String nomPays;
	protected int nbreHabitants;
	protected char categorie;
	
	public Ville() {
		System.out.println("Création d'une ville !");
		nomVille = "Inconnu";
		nomPays = "Inconnu";
		nbreHabitants = 0;
		this.setCategorie();
		nbreInstances++;
		nbreInstancesBis++;
	}
	public Ville(String pNom, int pNbre, String pPays) throws NombreHabitantException, NomVilleException		 
	{
		if(pNbre<0)
			throw new NombreHabitantException(pNbre);
		
		if(pNom.length() < 3)
			throw new NomVilleException("le nom de la ville est inférieur à 3 caractères ! nom = " + pNom);
		else
		{
			nomVille = pNom;
			nomPays = pPays;
			nbreHabitants = pNbre;
			this.setCategorie();
			nbreInstances++;
			nbreInstancesBis++;
		}
	}
	
		public static int getNombreInstancesBis()
	{
		return nbreInstancesBis;
	}
	public String getNom() {
		return nomVille;
	}
	public String getNomPays() {
		return nomPays;
	}
	public int getNombreHabitants()
	{
		return nbreHabitants;
	}
	public char getCategorie()
	{
		return categorie;
	}
	public void setNom(String pNom)
	{
		nomVille = pNom;
	}
	public void setNomPays(String pPays)
	{
		nomPays = pPays;
	}
	public void setNombreHabitants(int nbre)
	{
		nbreHabitants = nbre;
	}
	private void setCategorie()
	{
		int bornesSuperieures[] = {0, 1000, 10000, 100000, 500000, 1000000, 5000000, 10000000};
		char categories[] = {'?', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'};
		int i = 0;
		while (i < bornesSuperieures.length && this.nbreHabitants > bornesSuperieures[i])
			i++;
		this.categorie = categories[i];
	}
	
	public String decrisToi() {
		return "\t"+this.nomVille+" est une ville de "+this.nomPays+ ", elle comporte : "+this.nbreHabitants+" habitant(s) => elle est donc de catégorie : "+this.categorie;
	}
	
	public String comparer(Ville v1){
		String str = new String();
		
		if(v1.getNombreHabitants() > this.nbreHabitants)
			str = v1.getNom()+" est une ville plus peuplée que "+this.nomVille;
		
		else 
			str = this.nomVille+" est une ville plus peuplée que "+v1.getNom();
		return str;
	}
}