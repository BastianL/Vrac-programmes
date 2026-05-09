package interfacegraphique;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
public class ZoneGraphique extends Canvas implements MouseMotionListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private BarreEtat etat;
	private Color couleurChoisie;
	private int xInit, yInit;
	private Dessin dessin = new Dessin();
	
	public ZoneGraphique(BarreEtat etat) {
		addMouseListener(new GestionnaireClic(this));
		addMouseMotionListener(this);
		this.etat = etat;
		setBackground(Color.white);
		setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
	}
	public void initieDroite(int x, int y) {
		xInit = x;
		yInit = y;
		etat.afficheMessage("Relachez pour dessiner la droite");
	}
	public void termineDroite(int x, int y) {
		Graphics g = getGraphics();
		Droite droite = new Droite (xInit, yInit, x, y, couleurChoisie);
		dessin.ajouteDroite(droite);
		droite.dessineToi(g);
		etat.afficheMessage("Cliquez pour initier une droie");
	}
	public void mouseDragged(MouseEvent e) {
		// TODO Auto-generated method stub
		etat.afficheCoord(e.getX(), e.getY());
	}
	public void mouseMoved(MouseEvent e) {
		// TODO Auto-generated method stub
		etat.afficheCoord(e.getX(), e.getY());
	}
	public void efface() {
		dessin.efface();
		repaint();
	}
	public void defaire() {
		dessin.defaire();
		repaint();
	}
	
	public void changeCouleur(Color couleur) {
		couleurChoisie = couleur;
	}
	public void paint (Graphics g) {
		dessin.dessineToi(g);
	}
}
