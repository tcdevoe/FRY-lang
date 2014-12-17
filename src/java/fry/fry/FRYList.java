package fry;

import java.util.ArrayList;
import java.util.Collection;

public class FRYList<E> extends ArrayList<E> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public FRYList(){
		super();
	}
	
	public FRYList(Collection<? extends E> c){
		super(c);
	}
	
	public FRYList(int initialCapactity){
		super(initialCapactity);
	}
	
	
	@Override
	public E get(int index){
		try{
			return super.get(index);
		} catch (IndexOutOfBoundsException e){
			return null;
		}
	}
	

}
