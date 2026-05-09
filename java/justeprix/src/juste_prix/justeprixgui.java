package juste_prix;

import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import java.awt.Color;
import javax.swing.SwingConstants;
import javax.swing.JButton;
import javax.swing.JLabel;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;

public class justeprixgui {

	private JFrame frame;
	private JLabel txtEssaisRestants;
	private JLabel txtTempsRestant;
	private JTextField txtentree;
	private JLabel lblNewLabel;
	int i;
	private int temps;
	private boolean flag;
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					justeprixgui window = new justeprixgui();
					window.frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public justeprixgui() {
		initialize();
	}
	public void changetext() {
		txtEssaisRestants.setText("essais restants " + i);
	}
	
	public void changetexteprincipal(String texte) {
		lblNewLabel.setText(texte);
	}
	
	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setBackground(Color.GREEN);
		frame.getContentPane().setBackground(Color.GREEN);
		frame.setBounds(100, 100, 450, 300);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		
		JSplitPane splitPane = new JSplitPane();
		splitPane.setBounds(0, 0, 434, 25);
		splitPane.setBackground(Color.GREEN);
		frame.getContentPane().add(splitPane);
		flag = false;
		splitPane.setResizeWeight(0.5);
		System.out.println("fonction lancée");
		Random random = new Random();
		int justeprix = random.nextInt(100);
		txtTempsRestant = new JLabel();
		txtTempsRestant.setBackground(Color.GREEN);
		splitPane.setLeftComponent(txtTempsRestant);
		txtTempsRestant.setHorizontalAlignment(SwingConstants.LEFT);
		txtEssaisRestants = new JLabel();
		txtEssaisRestants.setHorizontalAlignment(SwingConstants.RIGHT);
		splitPane.setRightComponent(txtEssaisRestants);
		txtEssaisRestants.setBackground(new Color(0, 255, 0));
		txtEssaisRestants.setText("essais restants 5");
		temps = 60;
		Timer timer = new Timer();
		TimerTask task = new TimerTask() {

			@Override
			public void run() {
				// TODO Auto-generated method stub
				if (!flag) {
					if (temps > 0) {
						temps--; 
						txtTempsRestant.setText("Temps Restant" + temps);
						}
					else {
						lblNewLabel.setText("Perdu");
						i = 0;
					}
				}
			}
		};
		
		timer.scheduleAtFixedRate(task, 0, 1000);
		i = 5;
		JButton btnNewButton = new JButton("Valider");
		btnNewButton.setBounds(163, 204, 89, 23);
		btnNewButton.addMouseListener(
			new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent e) {
					if (i != 0) {
						int prixproposé = Integer.parseInt(txtentree.getText());				
						--i;
						if (prixproposé < justeprix ) {
							lblNewLabel.setText("c'est plus!");
							changetext();
						} else if (prixproposé > justeprix) {
							lblNewLabel.setText("c'est moins!");	
							changetext();
						}
						else {
							lblNewLabel.setText("vous avez gagné!");
							flag = true;
						}
					}
					else
					{
						lblNewLabel.setText("vous avez perdu!");
					}
				}
			});
	frame.getContentPane().add(btnNewButton);
	
	txtentree = new JTextField();
	txtentree.setBounds(138, 153, 139, 20);
	txtentree.setText("Entrez un nombre");
	frame.getContentPane().add(txtentree);
	txtentree.setColumns(10);
	lblNewLabel = new JLabel();
	lblNewLabel.setHorizontalAlignment(SwingConstants.CENTER);
	lblNewLabel.setBounds(138, 88, 139, 14);
	frame.getContentPane().add(lblNewLabel);
	}
}