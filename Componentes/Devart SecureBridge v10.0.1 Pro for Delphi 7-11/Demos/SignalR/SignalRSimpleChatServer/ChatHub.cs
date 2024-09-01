using System.Threading.Tasks;
using Microsoft.AspNetCore.SignalR;
using System.Collections.Generic;
using System;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace SignalRSimpleChat
{

    public class Chat : Hub
    {
 
        public async Task Send(string nick, string message)
        {
            Globals.Messages.Add(new Tuple<DateTime, string, string>(DateTime.Now, nick, message));
            await Clients.All.SendAsync("Send", DateTime.Now.ToString("HH:mm"), nick, message);
        }

        public async Task Connected(string nick)
        {
            Globals.UserList.Add(nick);
            await Clients.All.SendAsync("Connected", JsonConvert.SerializeObject(Globals.UserList), JsonConvert.SerializeObject(Globals.Messages, new IsoDateTimeConverter() { DateTimeFormat = "HH:mm" }));
        }

        public async Task Disconnected(string nick)
        {
            Globals.UserList.Remove(nick);
            await Clients.All.SendAsync("Disconnected", JsonConvert.SerializeObject(Globals.UserList));
        }

        public async Task Clear()
        {
            Globals.UserList.Clear();
            Globals.Messages.Clear();
        }

    }
}
