with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Chat_Messages;



-- ****   gnatmake -I/usr/local/ll/lib cleint.adb ******


procedure chat_client is
   package LLU renames Lower_Layer_UDP;
   package ASU renames Ada.Strings.Unbounded;
   package ACL renames Ada.Command_Line;
   package CM renames Chat_Messages;

   Server_EP: LLU.End_Point_Type;
   Client_EP: LLU.End_Point_Type;
   Buffer:    aliased LLU.Buffer_Type(1024);
   Request:   ASU.Unbounded_String;
   Reply:     ASU.Unbounded_String;
   Expired : Boolean;
   Nick_Name: ASU.Unbounded_String;
   Usage_Error: exception;
   Finish: Boolean;
   Comment: ASU.Unbounded_String;



   procedure Bild_ServerEP (Server_EP: out LLU.End_Point_Type) is
     Server_Port: Natural;
     Server_IP: ASU.Unbounded_String;
     Server_Name: ASU.Unbounded_String;
   begin
     Server_Name := ASU.To_Unbounded_String(ACL.Argument(1));
     Server_IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Server_Name)));
     Server_Port := Integer'Value(ACL.Argument(2));
		 Server_EP := LLU.Build(ASU.To_String(Server_IP), Server_Port);
   end Bild_ServerEP;


   procedure Read_Nick (Nick_Name: out ASU.Unbounded_String) is
   begin
     Nick_Name := ASU.To_Unbounded_String(ACL.Argument(3));
   end Read_Nick;

   procedure Build_Init (Nick_Name: in out ASU.Unbounded_String;
                            Client_EP: in LLU.End_Point_Type) is

   begin
    CM.Message_Type'Output(Buffer'Access, CM.Init);
    LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
    ASU.Unbounded_String'Output(Buffer'Access, Nick_Name);
   end Build_Init;


   procedure Build_Writer (Client_EP: in LLU.End_Point_Type;
                            Comment: in out ASU.Unbounded_String)is
   begin

    CM.Message_Type'Output(Buffer'Access, CM.Writer);
    LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
    ASU.Unbounded_String'Output(Buffer'Access, Comment);
   end Build_Writer;


   procedure Send_Buffer (Server_EP: in LLU.End_Point_Type) is

   begin
     LLU.Send(Server_EP, Buffer'Access);
     -- reinicializa el buffer para empezar a utilizarlo
     LLU.Reset(Buffer);
   end Send_Buffer;


begin

  if ACL.Argument_Count = 3 then
    -- Construye el End_Point en el que está atado el servidor
    Bild_ServerEP(Server_EP);
    -- lee el nick
    Read_Nick(Nick_Name);
   -- Construe un End_Point libre cualquiera y se ata a él
    LLU.Bind_Any(Client_EP);
   -- reinicializa el buffer para empezar a utilizarlo
    LLU.Reset(Buffer);



    --manda Init al server
    Build_Init(Nick_Name, Client_EP);
    Send_Buffer(Server_EP);



    Finish:= False;
    while not Finish loop
      Ada.Text_IO.Put("Message: ");
      Comment := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line);
      if ASU.To_String(Comment) /= ".quit" then
        Build_Writer(Client_EP, Comment);
        Send_Buffer(Server_EP);
      else
        -- reinicializa (vacía) el buffer para ahora recibir en él
         LLU.Reset(Buffer);
         Finish:= True;
      end if;
    end loop;






   -- espera 2.0 segundos a recibir algo dirigido al Client_EP
   --   . si llega antes, los datos recibidos van al Buffer
   --     y Expired queda a False
   --   . si pasados los 2.0 segundos no ha llegado nada, se abandona la
   --     espera y Expired queda a True
    LLU.Receive(Client_EP, Buffer'Access, 2.0, Expired);
    if Expired then
      Ada.Text_IO.Put_Line ("Plazo expirado");
     else
      -- saca del Buffer un Unbounded_String
      Reply := ASU.Unbounded_String'Input(Buffer'Access);
      Ada.Text_IO.Put("Respuesta: ");
      Ada.Text_IO.Put_Line(ASU.To_String(Reply));
    end if;

   -- termina Lower_Layer_UDP
    LLU.Finalize;
  else
    raise Usage_Error;
  end if;

exception
  when Usage_Error =>
		Ada.Text_IO.Put_Line("Use: ./chat_client <server name> <server port> <nickname>");
		LLU.Finalize;

   when Ex:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
      LLU.Finalize;

end chat_client;
