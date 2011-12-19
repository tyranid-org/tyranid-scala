package org.tyranid.session;

import java.util.*;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;


public class MockHttpSession implements HttpSession {
  private String id = null;
  private Map<String, Object> attributes = new HashMap<String, Object>();
  private long creationTime;
  private int maxInactiveInterval = 30 * 60 * 1000;

  public MockHttpSession() {
    creationTime = System.currentTimeMillis();
  }

  public MockHttpSession( String id ) {
    this.id = id;
    creationTime = System.currentTimeMillis();
  }

  public long getCreationTime() {
    return creationTime;
  }

  public String getId() {
    if ( id == null ) {
      System.out.println("Inventing data in MockHttpSession.getId() to remain plausible.");
      id = "mock";
    }

    return id;
  }

  public long getLastAccessedTime() {
    return creationTime;
  }

  public ServletContext getServletContext() {
    return null;
  }

  public void setMaxInactiveInterval( int maxInactiveInterval ) {
    this.maxInactiveInterval = maxInactiveInterval;
  }

  public int getMaxInactiveInterval() {
    return maxInactiveInterval;
  }

  @SuppressWarnings({"UnnecessaryFullyQualifiedName"})
  @Deprecated
  public javax.servlet.http.HttpSessionContext getSessionContext() {
    return null;
  }

  public Object getAttribute( String name ) {
    return attributes.get( name );
  }

  @Deprecated
  public Object getValue( String name ) {
    return attributes.get(name);
  }

  public Enumeration<String> getAttributeNames() {
    return Collections.enumeration( attributes.keySet() );
  }

  @Deprecated
  public String[] getValueNames() {
    return attributes.keySet().toArray(new String[attributes.keySet().size()]);
  }

  public void setAttribute( String name, Object value ) {
    attributes.put(name, value);
  }

  @Deprecated
  public void putValue( String name, Object value ) {
    attributes.put(name, value);
  }

  public void removeAttribute( String name ) {
    attributes.remove( name );
  }

  @Deprecated
  public void removeValue( String name ) {
    attributes.remove( name );
  }

  public void invalidate() {
  }

  public boolean isNew() {
    return true;
  }
}