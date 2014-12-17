package fry;

import java.io.BufferedReader;

public class InputFile {
	public BufferedReader buf;
	public String delim;
	
	public InputFile(BufferedReader buf, String delim) {
		this.buf = buf;
		this.delim = delim;
	}
}
