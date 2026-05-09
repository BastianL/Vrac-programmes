package test;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.List;

public class Garage {
	protected List<Vehicule> voiture;
	
	public Garage() {
		File f = new File("Garage.txt");
		this.voiture = null;
	}
	
	public Garage(List<Vehicule> pVoiture) {
		File f = new File("Garage.txt");
		this.voiture = pVoiture;
	}
	
	public String toString() {
		try {
		return voiture.toString();
		} catch(NullPointerException e) {
			return "vous_navez_pas_de_voiture";
		}
		
		finally {
			
		}
	}

	public void addVoiture(Vehicule voit) {
		voiture.add(voit);
		try {
			ObjectOutputStream oos = new ObjectOutputStream(
					new BufferedOutputStream(
							new FileOutputStream(
									"Garage.txt")));
			oos.writeObject(voit);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}}
