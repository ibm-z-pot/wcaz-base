package com.ibm.wcaz.implementation;

import com.ibm.jzos.fields.CobolDatatypeFactory;
import com.ibm.jzos.fields.StringField;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Objects;

public class Dfhcommarea implements Cloneable, Comparable<Dfhcommarea> {
    private static final Charset encoding = Charset.forName("IBM-1047");
    
    private String progaMessage = "";
    
    /** Initialize fields to non-null default values */
    public Dfhcommarea() {}
    
    /** Initialize all fields to provided values */
    public Dfhcommarea(String progaMessage) {
        this.progaMessage = progaMessage;
    }
    
    @Override
    public Dfhcommarea clone() throws CloneNotSupportedException {
        Dfhcommarea cloned = (Dfhcommarea) super.clone();
        return cloned;
    }
    
    /**
     * Initialize {@code Dfhcommarea} using a COBOL-formatted byte array
     * @see #setBytes(byte[], int)
     */
    protected Dfhcommarea(byte[] bytes, int offset) {
        setBytes(bytes, offset);
    }
    
    /**
     * Initialize {@code Dfhcommarea} using a COBOL-formatted byte array
     * starting at the beginning of the array
     * @see #setBytes(byte[], int)
     */
    protected Dfhcommarea(byte[] bytes) {
        this(bytes, 0);
    }
    
    /**
     * Deserialize the COBOL-formatted byte array into a new {@code Dfhcommarea} object
     * @see #setBytes(byte[], int)
     */
    public static Dfhcommarea fromBytes(byte[] bytes, int offset) {
        return new Dfhcommarea(bytes, offset);
    }
    
    /**
     * Deserialize the COBOL-formatted byte array into a new {@code Dfhcommarea} object
     * starting at the beginning of the array
     * @see #setBytes(byte[], int)
     */
    public static Dfhcommarea fromBytes(byte[] bytes) {
        return fromBytes(bytes, 0);
    }
    
    /**
     * Deserialize the COBOL-formatted string into a new {@code Dfhcommarea} object
     * The string is first converted to a byte array using the IBM-1047 EBCDIC encoding.
     * @see #setBytes(byte[], int)
     */
    public static Dfhcommarea fromBytes(String bytes) {
        return fromBytes(bytes.getBytes(encoding));
    }
    
    public String getProgaMessage() {
        return this.progaMessage;
    }
    
    public void setProgaMessage(String progaMessage) {
        this.progaMessage = progaMessage;
    }
    
    /**
     * Set all fields to an empty value.
     * The empty value is trimmed spaces for {@code String} and {@code char} fields and zero for numeric fields, recursively applied to classes and arrays.
     */
    public void reset() {
        progaMessage = "";
    }
    
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("{ progaMessage=\"");
        s.append(getProgaMessage());
        s.append("\"");
        s.append("}");
        return s.toString();
    }
    
    private boolean equals(Dfhcommarea that) {
        return this.progaMessage.equals(that.progaMessage);
    }
    
    /** Per-field equality comparison; only returns equal if argument is the same class */
    @Override
    public boolean equals(Object that) {
        return (that instanceof Dfhcommarea other) && other.canEqual(this) && this.equals(other);
    }
    
    /** Does {@code that} have the same runtime type as {@code this}? */
    public boolean canEqual(Object that) {
        return that instanceof Dfhcommarea;
    }
    
    @Override
    public int hashCode() {
        int result = 17;
        result = 31 * result + Objects.hashCode(progaMessage);
        return result;
    }
    
    @Override
    public int compareTo(Dfhcommarea that) {
        int c = 0;
        c = this.progaMessage.compareTo(that.progaMessage);
        if ( c == 0 && !(that.canEqual(this) && this.canEqual(that)) ) c = this.getClass().getTypeName().compareTo(that.getClass().getTypeName());
        return c;
    }
    
    // Start of COBOL-compatible binary serialization metadata
    private static CobolDatatypeFactory factory = new CobolDatatypeFactory();
    static {
        factory.setStringTrimDefault(true);
        factory.setStringEncoding(encoding.name());
    }
    
    private static final StringField PROGA_MESSAGE = factory.getStringField(32);
    public static final int SIZE = factory.getOffset();
    // End of COBOL-compatible binary serialization metadata
    
    /**
     * Retrieves a COBOL-format byte array representation of the {@code Dfhcommarea} object
     * @param bytes  preallocated byte array to store the object serialization
     * @param offset offset in the byte array where serialization should begin
     * @return the byte array, updated with the newly added data formatted as in the {@code DFHCOMMAREA} record
     * @see "DFHCOMMAREA record at PROGB.cbl:17"
     */
    public byte[] getBytes(byte[] bytes, int offset) {
        PROGA_MESSAGE.putString(progaMessage, bytes, offset);
        return bytes;
    }
    
    /**
     * Retrieves a COBOL-format byte array representation of the {@code Dfhcommarea} object,
     * starting at the beginning of the array
     * @see #getBytes(byte[], int)
     */
    public final byte[] getBytes(byte[] bytes) {
        return getBytes(bytes, 0);
    }
    
    /**
     * Retrieves a COBOL-format byte array representation of the {@code Dfhcommarea} object,
     * allocating a new array
     * @see #getBytes(byte[], int)
     */
    public final byte[] getBytes() {
        return getBytes(new byte[numBytes()]);
    }
    
    /**
     * Retrieves a COBOL-format string representation of the {@code Dfhcommarea} object
     * @return the result of {@link #getBytes()} interpreted using the IBM-1047 EBCDIC encoding
     */
    public final String toByteString() {
        return new String(getBytes(), encoding);
    }
    
    /**
     * Updates the fields of this instance from a COBOL-format byte array
     * @param bytes  byte array formatted as in the {@code DFHCOMMAREA} record; will be space-padded to length if necessary
     * @param offset offset in the byte array where deserialization should begin
     * @see "DFHCOMMAREA record at PROGB.cbl:17"
     */
    public void setBytes(byte[] bytes, int offset) {
        if (bytes.length < SIZE + offset) {
            byte[] newBytes = Arrays.copyOf(bytes, SIZE + offset);
            Arrays.fill(newBytes, bytes.length, SIZE + offset, (byte)0x40 /*default EBCDIC space character*/);
            bytes = newBytes;
        }
        progaMessage = PROGA_MESSAGE.getString(bytes, offset);
    }
    
    
    /**
     * Updates the fields of this instance from a COBOL-format byte array,
     * starting at the beginning of the array
     * @see #setBytes(byte[], int)
     */
    public final void setBytes(byte[] bytes) {
        setBytes(bytes, 0);
    }
    
    /**
     * Updates the fields of this instance from a COBOL-format string.
     * The string is first converted to a byte array using the IBM-1047 EBCDIC encoding.
     * @see #setBytes(byte[], int)
     */
    public final void setBytes(String bytes) {
        setBytes(bytes.getBytes(encoding));
    }
    
    /** The number of bytes required for a COBOL-format byte array representing this object */
    public int numBytes() {
        return SIZE;
    }
    
}
