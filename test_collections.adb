with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.IO_Exceptions;

with Client_Collections;
with Lower_Layer_UDP;

procedure Test_Collections is
	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;
	
	List : Client_Collections.Collection_Type;

	Port : Natural := 6000;
	function Build_EP return LLU.End_Point_Type is
	begin
		Port := Port + 1;
		return LLU.Build("127.0.0.1", Port);
	end;

	procedure Add_Client (Nick : in ASU.Unbounded_String; Unique : in Boolean) is
      EP : LLU.End_Point_Type := Build_EP;
	begin
		Ada.Text_IO.Put_Line ("Adding " & ASU.To_String(Nick) & " [" & Boolean'Image(Unique) & "]" & " :" & Natural'Image(Port));
		Client_Collections.Add_Client (List, EP, Nick, Unique);	
	exception
		when Client_Collections.Client_Collection_Error =>
			Ada.Text_IO.Put_Line ("Exception adding " & ASU.To_String(Nick) & " [" & Boolean'Image(Unique) & "]");
	end;

	procedure Delete_Client (Nick : in ASU.Unbounded_String) is
	begin
		Ada.Text_IO.Put_Line ("Deleting " & ASU.To_String(Nick));
		Client_Collections.Delete_Client (List, Nick);	
	exception
		when Client_Collections.Client_Collection_Error =>
			Ada.Text_IO.Put_Line ("Exception deleting " & ASU.To_String(Nick));
	end;

	procedure Search_Client (Port : in Natural) is
      EP : LLU.End_Point_Type;
	begin
      EP := LLU.Build("127.0.0.1", Port);
		Ada.Text_IO.Put_Line ("Client at " & Natural'Image(Port) & " -> " & ASU.To_String(Client_Collections.Search_Client (List, EP)));	
	exception
		when Client_Collections.Client_Collection_Error =>
			Ada.Text_IO.Put_Line ("Client not found at " & Natural'Image(Port));
	end;

begin
	Add_Client (ASU.To_Unbounded_String("pedro"), false);
	Add_Client (ASU.To_Unbounded_String("ana"), false);
	Add_Client (ASU.To_Unbounded_String("carlos"), false);
	Add_Client (ASU.To_Unbounded_String("pedro"), false);
	Add_Client (ASU.To_Unbounded_String("jorge"), false);
	Add_Client (ASU.To_Unbounded_String("pedro"), true);

   Delete_Client (ASU.To_Unbounded_String("paul"));
   Delete_Client (ASU.To_Unbounded_String("pedro"));
   Delete_Client (ASU.To_Unbounded_String("pedro"));
   Delete_Client (ASU.To_Unbounded_String("pedro"));

   Search_Client (6002);
   Search_Client (6001);

	Ada.Text_IO.Put_Line (Client_Collections.Collection_Image(List));

	LLU.Finalize;

exception
    when Ex:others =>
		Ada.Text_IO.Put_Line ("Exception: " &
				Ada.Exceptions.Exception_Name(Ex) & " : " &
				Ada.Exceptions.Exception_Message(Ex));
		LLU.Finalize;

end Test_Collections;
