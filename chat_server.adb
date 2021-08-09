with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;


-- ****   gnatmake -I/usr/local/ll/lib cleint.adb ******


procedure Server is
   package LLU renames Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
   package ACL renames Ada.Command_Line;

   Server_EP: LLU.End_Point_Type;
   Client_EP: LLU.End_Point_Type;
   Buffer:    aliased LLU.Buffer_Type(1024);
   Request: ASU.Unbounded_String;
   Reply: ASU.Unbounded_String := ASU.To_Unbounded_String ("¡Bienvenido!");
   Expired : Boolean;
   Usage_Error: exception;


   procedure Bild_ServeEP (Server_EP: out LLU.End_Point_Type) is
     Server_Port: Natural;
     Server_IP: ASU.Unbounded_String;
   begin
     Server_Port := Integer'Value(ACL.Argument(1));
     Server_IP := ASU.To_Unbounded_String(LLU.To_IP(LLU.Get_Host_Name));
     Server_EP := LLU.Build(ASU.To_String(Server_IP), Server_Port);
     --Server_EP := LLU.Build ("127.0.0.1", 6123);
     LLU.Bind(Server_EP);
   end Bild_ServeEP;

begin

    if ACL.Argument_Count = 1 then
      Bild_ServeEP(Server_EP);
      loop
        -- reinicializa (vacía) el buffer para ahora recibir en él
        LLU.Reset(Buffer);

        -- espera 1000.0 segundos a recibir algo dirigido al Server_EP
        --   . si llega antes, los datos recibidos van al Buffer
        --     y Expired queda a False
        --   . si pasados los 1000.0 segundos no ha llegado nada, se abandona
        --     la espera y Expired queda a True
        LLU.Receive (Server_EP, Buffer'Access, 1000.0, Expired);

        if Expired then
           Ada.Text_IO.Put_Line ("Plazo expirado, vuelvo a intentarlo");
        else
           -- saca
           Client_EP := LLU.End_Point_Type'Input (Buffer'Access);
           Request := ASU.Unbounded_String'Input (Buffer'Access);
           Ada.Text_IO.Put ("Petición: ");
           Ada.Text_IO.Put_Line (ASU.To_String(Request));

           -- reinicializa (vacía) el buffer
           LLU.Reset (Buffer);

           --  introduce el Unbounded_String en el Buffer
           ASU.Unbounded_String'Output (Buffer'Access, Reply);

           -- envía el contenido del Buffer
           LLU.Send (Client_EP, Buffer'Access);
        end if;
      end loop;
    else
      raise Usage_Error;
    end if;

exception
  when Usage_Error =>
		Ada.Text_IO.Put_Line("Use: ./chat_sever <server port>");
		LLU.Finalize;

  when Ex:others =>
    Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                          Ada.Exceptions.Exception_Name(Ex) & " en: " &
                          Ada.Exceptions.Exception_Message(Ex));
    LLU.Finalize;

end Server;
