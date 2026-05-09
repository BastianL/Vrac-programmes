package juste_prix;

import java.awt.EventQueue;

import javax.swing.JFrame;
import java.awt.BorderLayout;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import java.awt.Color;
import javax.swing.SwingConstants;
import javax.swing.JButton;
import javax.swing.JLabel;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Random;
import java.util.Scanner;

public class justeprixgui {

	private JFrame frame;
	private JLabel txtEssaisRestants;
	private JLabel txtTempsRestant;
	private JTextField txtFds;
	private static String texte;

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
		
		txtEssaisRestants = new JLabel();
		txtEssaisRestants.setBackground(new Color(0, 255, 0));
		txtEssaisRestants.setText(" essais restants");
		splitPane.setResizeWeight(0.5);
		splitPane.setLeftComponent(txtEssaisRestants);
		
		txtTempsRestant = new JLabel();
		txtTempsRestant.setBackground(Color.GREEN);
		txtTempsRestant.setText("Temps Restant");
		splitPane.setRightComponent(txtTempsRestant);
		txtTempsRestant.setHorizontalAlignment(SwingConstants.RIGHT);
		
		JButton btnNewButton = new JButton("Valider");
		btnNewButton.setBounds(163, 204, 89, 23);
		btnNewButton.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {				
				int essai = Integer.parseInt(txtFds.getText());
				System.out.println("fonction lancée");
				Random random = new Random();
				int justeprix = random.nextInt(100);
				Scanner entree = new Scanner(System.in);
				for ( ; essai!=0; essai--) {
					System.out.println("fonction pénétrée");
					texte = "essais restants" + essai;
				}
				System.out.println ("boucle sautée");
		}});
		frame.getContentPane().add(btnNewButton);
		
		txtFds = new JTextField();
		txtFds.setBounds(138, 153, 139, 20);
		txtFds.setText("fds");
		frame.getContentPane().add(txtFds);
		txtFds.setColumns(10);
		
		texte = "entrez un nombre d'essai";
		JLabel lblNewLabel = new JLabel(texte);
		lblNewLabel.setBounds(112, 101, 165, 14);
		frame.getContentPane().add(lblNewLabel);
	}
	
	private static boolean jeu(int essai) {
		System.out.println("fonction lancée");
		Random random = new Random();
		int justeprix = random.nextInt(100);
		Scanner entree = new Scanner(System.in);
		for ( ; essai==0; essai--) {
			System.out.println("fonction pénétrée");
			texte = "essais restants" + essai;
			int prixproposé = entree.nextInt();
			if (prixproposé < justeprix ) {
				System.out.println("c'est plus");
			} else if (prixproposé > justeprix) {
				System.out.println("c'est moins");
			}
			else {
				System.out.println("vous avez gagné");
				return true;
			}
			
		}
		entree.close();
		System.out.println("boucle sautée");
		return false;
	}
}
