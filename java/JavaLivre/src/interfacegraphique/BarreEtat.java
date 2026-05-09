package interfacegraphique;
import java.awt.*;
public class BarreEtat extends Panel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Label coord, info;
	public BarreEtat() {
		setBackground(Color.lightGray);
		setLayout(new GridLayout(1,2));
		add(info = new Label());
		add(coord = new Label());
	}
	public void afficheMessage(String message) {
		info.setText(message);
	}
	public void afficheCoord(int x, int y) {
		coord.setText("x:"+x+" ,y:"+y);
	}
}