package fry;

import java.util.ArrayList;
import java.util.Arrays;

public class FRYListFactory{

	public static FRYList<Integer> getGeneratedFryList(int min, int max){
		FRYList<Integer> list = new FRYList<Integer>(max-min);
		for(int i = min; i <= max; i++){
			list.add(new Integer(i));
		}
		return list;
	}

}