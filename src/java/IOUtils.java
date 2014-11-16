/***********************************

IOUtils.java contains functions for
reading and writing

************************************/

import java.io.PrintStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class IOUtils {

	// Writes to the output stream specified
	public static void Write(String outputSpec, String text){

		PrintStream o = null;
		// OutputSpec should be stdout, stdin, or string path to file
		if ( outputSpec.equals("stdout") ){
			o = System.out;
		} 
		else if ( outputSpec.equals("stderr") ){
			o = System.err;
		} 
		else {
			try {
				o = new PrintStream(new File(outputSpec));
			} 
			catch (FileNotFoundException e) {
				System.err.println("File " + outputSpec + " not found.\n" + e.getMessage());
				System.exit(1);
			}
		}
		o.println(text);
		o.flush();
	}

	public static void Read(String inputSpec, String text) throws IOException{

		BufferedReader i = null;
		String line = null;
		// inputSpec should be stdin or string path to file
		if ( inputSpec.equals("stdin")){
			i = new BufferedReader( new InputStreamReader(System.in));
		}
		else {
			try{
				i = new BufferedReader( new FileReader(new File(inputSpec)));
			}
			catch (FileNotFoundException e) {
				System.err.println("File " + inputSpec + " not found.\n" + e.getMessage());
				System.exit(1);
			}
		}

		while ( (line = i.readLine()) != null ){

		}

	}

}