package interfacegraphique;

import java.awt.*;

public class Droite {
	int xInit, yInit, xFin, yFin;
	Color couleur;
	public Droite(int xI, int yI, int xF, int yF, Color c) {
		xInit = xI;
		yInit = yI;
		xFin = xF;
		yFin = yF;
		couleur = c;
	}
	public void  dessineToi(Graphics g) {
		g.setColor(couleur);
		g.drawLine(xInit, yInit, xFin, yFin);
	}
}
