/***********************************

IOUtils.java contains functions for
reading and writing to input/output 
streams

************************************/
package fry;

import java.io.PrintStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class IOUtils {

	public static String stdout = "stdout";
	public static String stderr = "stderr";
	public static String stdin = "stdin";

	public static void Write(String outputSpec, int expr){
		Write(outputSpec, Integer.toString(expr));
	}

	public static void Write(String outputSpec, float expr){
		Write(outputSpec, Float.toString(expr));
	}

	public static void Write(String outputSpec, boolean expr){
		if (expr)
			Write(outputSpec, "true");
		else
			Write(outputSpec, "false");
	}

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

	public static void Write(String outputSpec, Object obj){
		Write(outputSpec, obj.toString());
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