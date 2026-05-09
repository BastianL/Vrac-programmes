package juste_prix;
import java.awt.event.ActionEvent;
import java.util.Random;
import javax.swing.*;
import java.util.Scanner;
public class Juste_prix extends javax.swing.JFrame {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 5687599391012867122L;
	private JTextField entreeField;
	private JLabel intitulé;
	private JLabel essairestants;
	private JLabel secondesrestantes;
	private JButton validerbouton;
	private void initComponents() {
		entreeField = new javax.swing.JTextField();
		intitulé = new javax.swing.JLabel();
		essairestants = new javax.swing.JLabel();
		secondesrestantes = new javax.swing.JLabel();
		validerbouton = new javax.swing.JButton();
		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
		setTitle("Le juste prix");
		intitulé.setText("entrez un nombre de tentatives");
		validerbouton.setText("valider");
		validerbouton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				validerBoutonActionPerformed(evt);
			}
		});
		javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
		getContentPane().setLayout(layout);
		
	}
		
	
	private void validerBoutonActionPerformed(ActionEvent evt) {
		// TODO Auto-generated method stub
		
	}


	public static void main(String[] args) {
		JFrame f = new JFrame ("ma fenetre");
		f.setSize(300,100);
		JButton b = new JButton ("mon bouton");
		f.getContentPane().add(b);
		f.setVisible(true);
		Scanner essais = new Scanner(System.in);
		while (true) {
		System.out.println("bienvenue au jeu du juste prix! Entrez le nombre d'essai dont vous voulez disposer");
		jeu(essais.nextInt());
		}
	}
	
	public static boolean jeu(int essai) {
		System.out.println("fonction lancée");
		Random random = new Random();
		int justeprix = random.nextInt(100);
		Scanner entree = new Scanner(System.in);
		for (int i=essai; i==0; i--) {
			System.out.println ("essais restants" + i);
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
		entree.close();
		}
		return false;
	}
	
}
