using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Hosting;
using System;
using System.Collections.Generic;

namespace SignalRSimpleChat
{

    public static class Globals
    {
        public static List<string> UserList = new List<string>();
        public static List<Tuple<DateTime, string, string>> Messages = new List<Tuple<DateTime, string, string>>();
    }

    public class Program
    {

        public static void Main(string[] args)
        {
            BuildWebHost(args).Run();
        }

        public static IWebHost BuildWebHost(string[] args) =>
            WebHost.CreateDefaultBuilder(args)
                .UseStartup<Startup>()
                .Build();
    }
}
