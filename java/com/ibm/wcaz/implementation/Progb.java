package com.ibm.wcaz.implementation;

import com.ibm.cics.server.CommAreaHolder;

public class Progb {
    private static Progb progb = new Progb();

    /** Initialize fields to non-null default values */
    public Progb() {
    }

    public static Progb getInstance() {
        return progb;
    }

    public void invoke(Dfhcommarea dfhcommarea) {
        String progcMessage = "";
        String wsByeMsg = "Goodbye from COBOL";
        String wsHelloMsg = "Hello from COBOL";
        progcMessage = wsHelloMsg;
        System.out.println("PROGB  request= " + progcMessage);
        try {
            byte[] jdeclCommareaByteArray = dfhcommarea.toByteArray();
            byte[] jdeclProgramOutput = Task.getTask().link(jdeclCommareaByteArray, "PROGC");
            dfhcommarea.setBytes(jdeclProgramOutput);
            progcMessage = dfhcommarea.getProgcMessage();
        } catch (CicsConditionException e) {
            throw e;
        }
        System.out.println("PROGC response= " + progcMessage);
        dfhcommarea.setProgaMessage(wsByeMsg);
    }

    public static void main(CommAreaHolder holder) {
        Dfhcommarea dfhcommarea = Dfhcommarea.fromBytes(holder.getValue());
        getInstance().invoke(dfhcommarea);
        holder.setValue(dfhcommarea.getBytes());
    }
}
