package interfacegraphique;
import java.awt.*;

public class Bouton extends Button {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public Bouton(String libelle) {
		super(libelle);
	}
	public Dimension getPreferedSize() {
		return new Dimension(80,25);
	}
}
