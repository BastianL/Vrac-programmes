package interfacegraphique;

import java.awt.event.*;

public class GestionnaireClic extends MouseAdapter {
	private ZoneGraphique zone;
	public GestionnaireClic(ZoneGraphique zone) {
		this.zone = zone;
	}
	public void mousePressed(MouseEvent e) {
		zone.initieDroite(e.getX(), e.getY());
	}
	public void mouseReleaded(MouseEvent e) {
		zone.termineDroite(e.getX(), e.getY());
	}
}
