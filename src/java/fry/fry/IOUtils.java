/***********************************

IOUtils.java contains functions for
reading and writing to input/output 
streams

************************************/
package fry;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;


public class IOUtils {

	public static String stdout = "stdout";
	public static String stderr = "stderr";
	public static String stdin = "stdin";

	public static PrintWriter getPrintWriter(String outputSpec){
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
		return new PrintWriter(o);
	}
	
	
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
		PrintWriter o = getPrintWriter(outputSpec);
		System.out.println(text);
		o.write(text);
	}

	public static void Write(String outputSpec, Object obj){
		Write(outputSpec, obj.toString());
	}
	
	public static void Write(String outputSpec, FRYTable tab){
		Write(outputSpec, tab, ",");
	}
	
	public static void Write(String outputSpec, FRYTable tab, String d){
		Object[] rec = null;
		String out = "";
		PrintWriter o = getPrintWriter(outputSpec);
		while((rec = tab.readRecord()) != null ){
			out = "";
			for(int i = 0; i < rec.length; i++){
				if ( i == (rec.length - 1)){
					out += rec[i].toString();
				}
				else {
					out += rec[i].toString()+d;
				}
			}
			o.write(out+"\n");
		}
		o.close();
	}
	
	public static void WriteColumn(String outputSpec, ArrayList<Object> column){
		PrintWriter o = getPrintWriter(outputSpec);
		for(Object obj : column){
			o.write(obj.toString()+"\n");
		}
		o.close();
	}

	public static InputFile Read(String inputSpec, String delim) throws IOException{
		BufferedReader i = null;
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
		return new InputFile(i, delim);
	}

}