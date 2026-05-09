package interfacegraphique;
import java.awt.*;
import java.awt.event.*;
public class GereGraphique implements ActionListener, ItemListener {
	static final int QUITTER = 0;
	static final int NOUVEAU = 1;
	static final int DEFAIRE = 2;
	static final int APROPOS = 3;
	private int id;
	private ZoneGraphique zone;
	private Color couleurs[];
	public GereGraphique (int id, ZoneGraphique zone) {
		this.id = id;
		this.zone = zone;
	}
	public GereGraphique(Color couleurs[], ZoneGraphique zone) {
		this.couleurs = couleurs;
		this.zone = zone;
	}
	public void actionPerformed(ActionEvent e) {
		switch(id) {
		case QUITTER:
					System.exit(0);
					break;
		case NOUVEAU:
					zone.efface();
					break;
		case DEFAIRE:
					zone.defaire();
					break;
		case APROPOS:
					System.out.println("Non implementé");
		}
	}
	@Override
	public void itemStateChanged(ItemEvent e) {
		// TODO Auto-generated method stub
		Choice liste = (Choice) e.getSource();
		zone.changeCouleur(couleurs[liste.getSelectedIndex()]);
	}

}
