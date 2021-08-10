with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Chat_Messages;


-- ****   gnatmake -I/usr/local/ll/lib cleint.adb ******


procedure chat_server is
   package LLU renames Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
   package ACL renames Ada.Command_Line;
   package CM renames Chat_Messages;

   Server_EP: LLU.End_Point_Type;
   Client_EP: LLU.End_Point_Type;
   Buffer:    aliased LLU.Buffer_Type(1024);
   Request: ASU.Unbounded_String;
   Reply: ASU.Unbounded_String := ASU.To_Unbounded_String ("¡Bienvenido!");
   Expired : Boolean;
   Usage_Error: exception;
   Nick_Name: ASU.Unbounded_String;
   Message: CM.Message_Type;
   Comment: ASU.Unbounded_String;


   procedure Bild_ServeEP (Server_EP: out LLU.End_Point_Type) is
     Server_Port: Natural;
     Server_IP: ASU.Unbounded_String;
   begin
     Server_Port := Integer'Value(ACL.Argument(1));
     Server_IP := ASU.To_Unbounded_String(LLU.To_IP(LLU.Get_Host_Name));
     Server_EP := LLU.Build(ASU.To_String(Server_IP), Server_Port);
     LLU.Bind(Server_EP);
   end Bild_ServeEP;


   procedure Type_Message (Message: out CM.Message_Type) is
   begin
     Message:= CM.Message_Type'Input (Buffer'Access);
   end Type_Message;


   procedure Read_Init(Client_EP: out LLU.End_Point_Type;
                        Nick_Name: out ASU.Unbounded_String) is
   begin
     Ada.Text_IO.Put("INIT received from ");
     Client_EP := LLU.End_Point_Type'Input (Buffer'Access);
     Nick_Name := ASU.Unbounded_String'Input (Buffer'Access);
     Ada.Text_IO.Put_Line (ASU.To_String(Nick_Name));
   end Read_Init;


   procedure Read_Writer(Nick_Name: in ASU.Unbounded_String) is
   begin
     Ada.Text_IO.Put("WRITER received from ");
     Ada.Text_IO.Put(ASU.To_String(Nick_Name));
     Ada.Text_IO.Put(": ");
     Client_EP := LLU.End_Point_Type'Input (Buffer'Access);
     Comment := ASU.Unbounded_String'Input (Buffer'Access);
     Ada.Text_IO.Put_Line(ASU.To_String(Comment));
   end Read_Writer;


begin

    if ACL.Argument_Count = 1 then
      Bild_ServeEP(Server_EP);
      loop
        -- reinicializa (vacía) el buffer para ahora recibir en él
        LLU.Reset(Buffer);
        -- espera 1000.0 segundos a recibir algo dirigido al Server_EP
        LLU.Receive (Server_EP, Buffer'Access, 1000.0, Expired);
        Type_Message(Message);
        case Message is
          when CM.Init =>
            Read_Init(Client_EP, Nick_Name);
            LLU.Reset (Buffer);
          when CM.Writer =>
            Read_Writer(Nick_Name);
            LLU.Reset (Buffer);
          when others =>
            LLU.Reset (Buffer);
        end case;
        LLU.Reset (Buffer);
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

end chat_server;







--        if Expired then
--           Ada.Text_IO.Put_Line ("Plazo expirado, vuelvo a intentarlo");
--      else
--           Type_Message(Message);
--         case Message is
--           when CM.Init =>
--             Ada.Text_IO.Put("INIT received from ");
--             Ada.Text_IO.Put_Line (ASU.To_String(Nick_Name));
--             Client_EP := LLU.End_Point_Type'Input (Buffer'Access);
--             Nick_Name := ASU.Unbounded_String'Input (Buffer'Access);
               --Request := ASU.Unbounded_String'Input (Buffer'Access);



               -- reinicializa (vacía) el buffer
--               LLU.Reset (Buffer);

               --  introduce el Unbounded_String en el Buffer
--               ASU.Unbounded_String'Output (Buffer'Access, Reply);

               -- envía el contenido del Buffer
--               LLU.Send (Client_EP, Buffer'Access);
--            when others =>
--              LLU.Reset (Buffer);
--            end case;
--        end if;
