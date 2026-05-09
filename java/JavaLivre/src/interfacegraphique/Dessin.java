package interfacegraphique;
import java.awt.Graphics;
import java.util.*;
@SuppressWarnings("rawtypes")
public class Dessin extends java.util.Vector {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@SuppressWarnings("unchecked")
	public void ajouteDroite(Droite droite) {
		addElement (droite);
	}
	public void defaire() {
		if(!isEmpty())
			removeElement(lastElement());
	}
	public void efface() {
		removeAllElements();
	}
	public void dessineToi(Graphics g) {
		Droite droite;
		for(Enumeration<?> e = elements(); e.hasMoreElements();) {
			droite = (Droite) e.nextElement();
			droite.dessineToi(g);
		}
	}
}
