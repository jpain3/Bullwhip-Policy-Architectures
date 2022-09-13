import csv
import datetime
import random
import tensorflow as tf
import os
import numpy as np
import matplotlib.pyplot as plt
from gym import Env
from gym.spaces import Discrete, Box

# https://github.com/jmpf2018/ShipAI

## Creating Environment

class BeerGameEnv(Env):

    def __init__(self):
        # Define the action and observation spaces
        # Definition of the limits on orders the AI agent can make

        self.AI_Entity = True
        self.AI_Order_Plus = True              # If set to true, then the agent order is relative to the incoming order
                                                #    and the action_space above should reflect that by spanning both
                                                #    positive and negative values.

        if self.AI_Order_Plus != True:

            MaxOrder = 50

            self.action_space = spaces.Discrete(MaxOrder+1)  # a discrete array representing the possible entity order choices,
                                                             #   starting from 0
        else:

            Relative_Min_Order = -20                        # Amount relative to the incoming the order the agent
            Relative_Max_Order = +20                        #   can order itself

            #Force sign conventions on Relative_ Max_ or Min_Order just in case
            self.Relative_Min_Order = (-1)*np.sign(Relative_Min_Order)*Relative_Min_Order
            self.Relative_Max_Order = np.sign(Relative_Max_Order)*Relative_Max_Order

            #Determine action space full span (including 0)
            Action_Space_Span = (-1)*self.Relative_Min_Order+self.Relative_Max_Order+1

            self.action_space = spaces.Discrete(Action_Space_Span)



        # Set Global state parameters

        self.Random_Teams = False
        self.Bootstrapped_Teams = True
        self.Fixed_Team_Nb = False      # Team number to place the AI on.
                                    #   If Random_Teams flag is True or the Bootstrapped_teams = True, then is is ignored

        self.Random_AI_Position = False
        self.AI_Position = 0      # Position in the supply chain to place the AI in, between 0 and 3.
                                    #   If Random_AI_Position flag is True, then is is ignored

        self.Random_Horizon = True
        self.min_horizon = 36      # If the Horizon is random, the lower bound on the horizon length
        self.max_horizon = 68      # If the Horizon is random, the upper bound on the horizon length
        self.fixed_horizon = 52    # Fixed horizon, only used if above Random_Horizon flag is set to False

        self.Integer_Ordering = False
        self.Noisey_Ordering = False

        # Customer Order String
        # Classic Beer Game
        Step_Round = 4
        self.Orders = ([4] * Step_Round) + ([9] * (1000 - Step_Round))
        Second_Step = 150
        self.Orders = self.Orders[0:Second_Step] + ([9] * (1000 - Second_Step))

        # Finanical and Physical layout of the game
        self.Holding_Cost = 0.50
        self.Backorder_Cost = 1.00
        self.Initial_OrderFlows = self.Orders[0]
        self.Initial_Inventory = 12
        self.Information_Delay = 2
        self.Shipping_Delay = 2

        # State space for the problem. Need to decide the scale of this.... what can the AI see and remember?

        Agent_Sees = {'Agent_Order_Received': Box(0, np.inf, shape=(1,)),
                      'Agent_OH_Inventory': Box(0, np.inf, shape=(1,)),
                      'Agent_Backorder': Box(0, np.inf, shape=(1,)),
                      'Agent_Recent_Order': Box(0, np.inf, shape=(1,)),
                      'period': Box(0, 1000, shape=(1,)),
                      # NOTE: This could be bounded by np.inf but realistically t=1000 is an order of magnitude larger than the expected maximum time horizon
                      'AI_Entity_Index': Box(0, 3, shape=(1,)),
                      #Below are observations of the estimate of the ordering parameters in the model of the other players
                      # For this 4 parameter model of ordering behavior, with three other players in the system, this adds 12 parameters to the observable space
                      'OtherPlayer1_theta': Box(0,1,shape=(1,)),
                      'OtherPlayer2_theta': Box(0, 1, shape=(1,)),
                      'OtherPlayer3_theta': Box(0, 1, shape=(1,)),
                      'OtherPlayer1_alphas_s': Box(0, 1, shape=(1,)),
                      'OtherPlayer2_alphas_s': Box(0, 1, shape=(1,)),
                      'OtherPlayer3_alphas_s': Box(0, 1, shape=(1,)),
                      'OtherPlayer1_beta': Box(0, 1, shape=(1,)),
                      'OtherPlayer2_beta': Box(0, 1, shape=(1,)),
                      'OtherPlayer3_beta': Box(0, 1, shape=(1,)),
                      'OtherPlayer1_S_prime': Box(0, np.inf, shape=(1, )),
                      'OtherPlayer2_S_prime': Box(0, np.inf, shape=(1, )),
                      'OtherPlayer3_S_prime': Box(0, np.inf, shape=(1, ))
                      # NOTE: This should be upper bounded by the largest possible entity index (reminder that Python indexes at 0)
                      }

        # self.observation_space = gym.spaces.Dict(Agent_Sees)

        # State space coerced into a box shape to better work with Keras syntax, note that the ShippingFlows are two
        # different items here. Furthermore, note that this is defined via the Box shape, which is continuous, even
        # though some of these observations may always be discrete (AI_Entity_Index) as an example
        obs_space = spaces.Box(low=np.array([0, 0, 0, 0, 0, 0,
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
                               high=np.array([np.inf, np.inf, np.inf, np.inf, 1000, 3,
                                              0, 0, 0, 0, 0, 0, 0, 0, 0, np.inf, np.inf, np.inf]))

        self.observation_space = obs_space

        #Team_Parameter_Filename = "JS Parameter Table.csv"
        Team_Parameter_Filename = "Combined Historic Fitted JS Parameter Table.csv"

        with open(Team_Parameter_Filename, newline='') as csvfile:
            Team_Parameter_Data = list(csv.reader(csvfile))
            All_Team_Parameters = np.asarray(Team_Parameter_Data)

        # Remove header row
        Team_Parameter_Header = All_Team_Parameters[0, :]
        All_Team_Parameters = np.delete(All_Team_Parameters, (0), axis=0)

        # Replace all blanks with 0's
        All_Team_Parameters = np.asarray([[x or '0' for x in xs] for xs in All_Team_Parameters])

        # Extract the team numbers and convert to integers or numbers from strings as appropriate
        self.Team_Index = [int(item) for item in np.ndarray.tolist(All_Team_Parameters[:, 1])]
        self.Team_Name = np.ndarray.tolist(All_Team_Parameters[:, 0])
        self.Entity_Code = np.ndarray.tolist(All_Team_Parameters[:, 2])
        self.Entity_Index = [int(item) for item in np.ndarray.tolist(All_Team_Parameters[:, 3])]
        self.thetas = [float(item) for item in np.ndarray.tolist(All_Team_Parameters[:, 4])]
        self.alphas = [float(item) for item in np.ndarray.tolist(All_Team_Parameters[:, 5])]
        self.betas = [float(item) for item in np.ndarray.tolist(All_Team_Parameters[:, 6])]
        self.S_primes = [float(item) for item in np.ndarray.tolist(All_Team_Parameters[:, 7])]

        if self.Fixed_Team_Nb >= min(self.Team_Index) and self.Fixed_Team_Nb <= max(self.Team_Index):

            Match_Team = self.Fixed_Team_Nb

            # Create a mask of the rows that correspond to that team number
            Match_Team_Mask = np.asarray(self.Team_Index) == Match_Team

            # Filter, using the mask, the arrays that have the data for the team that was drawn
            Match_Team_Theta = np.asarray(self.thetas)[Match_Team_Mask]
            Match_Team_Alpha = np.asarray(self.alphas)[Match_Team_Mask]
            Match_Team_Beta = np.asarray(self.betas)[Match_Team_Mask]
            Match_Team_S_prime = np.asarray(self.S_primes)[Match_Team_Mask]

            # Assemble the team parameters into a named list for later use in the main Beer Game function
            Match_Team_Parameter_df = {"theta": np.ndarray.tolist(Match_Team_Theta),
                                       "alpha_s": np.ndarray.tolist(Match_Team_Alpha),
                                       "beta": np.ndarray.tolist(Match_Team_Beta),
                                       "S_prime": np.ndarray.tolist(Match_Team_S_prime)}

            self.Parameter_df = Match_Team_Parameter_df
        else:
            # Set the defualt ordering parameter values (for use if the random team flag above is False or no team number is provided)
            self.Parameter_df = {"theta": [0.36] * 4,
                                 "alpha_s": [0.26] * 4,
                                 "beta": [0.34] * 4,
                                 "S_prime": [17] * 4}

    # Main function that runs the beer game for a single step
    def PirateBeerGame_funct(self, AI_Entity_Index, AI_Order, Orders, Order_flows, Shipping_flows, OH_Inventory,
                             Backorder, L_hat, Production_Request, AI_Entity=False, AI_parameter_df=False,
                             Integer_Ordering=False, Noisey_Ordering=False, Noise_Mean=0, Noise_SD=1,
                             Parameter_df=False, Shipping_Delay=2, Information_Delay=2) -> object:

        Relative_Ordering = self.AI_Order_Plus

        #If relative ordering is true, translate the agent action into a relative number
        if Relative_Ordering == True:
            AI_Relative_Order = AI_Order + self.Relative_Min_Order # + 1

        Final_Orders = np.empty(4, dtype=float)
        OH_Inventory = np.array(OH_Inventory)
        Shipping_flows = np.array(Shipping_flows)
        Order_flows = np.array(Order_flows)

        # Ensure that the order flow facing the retailer is the actual customer order
        Order_flows[0, 0] = Orders

        # Read in the ordering paramters
        if Parameter_df != False:
            theta = Parameter_df['theta']
            alpha_s = Parameter_df['alpha_s']
            beta = Parameter_df['beta']
            S_prime = Parameter_df['S_prime']
        else:
            theta = [0.36] * 4
            alpha_s = [0.26] * 4
            beta = [0.34] * 4
            S_prime = [17] * 4

            TeamName = "Default Average Agents"

        # Read in AI Ordering Parameters if present
        if AI_parameter_df != False:
            theta[AI_Entity_Index] = AI_parameter_df['theta']
            alpha_s[AI_Entity_Index] = AI_parameter_df['alpha_s']
            beta[AI_Entity_Index] = AI_parameter_df['beta']
            S_prime[AI_Entity_Index] = AI_parameter_df['S_prime']

        #####Recieve Inventory and Advance Shipping Delays#####

        # Recieve shipments
        New_OH_Inventory = OH_Inventory + Shipping_flows[:, 0]

        # Advance shippping delays
        Shipping_flows[:, 0] = Shipping_flows[:, (Shipping_Delay - 1)]
        # Shipping_flows[:, (Shipping_Delay - 1)] = np.nan

        #####Fill Orders######

        # View Orders
        Order_Received = Order_flows[:, 0]
        # Calculate net order that needs to be fullfilled
        Incoming_Order = Order_flows[:, 0] + Backorder
        # Ship what you can
        Outbound_shipments = np.maximum(0, np.minimum(New_OH_Inventory, Incoming_Order))

        # Put shipments into lefthand shipping slot
        Shipping_flows[0:3, 1] = Outbound_shipments[1:]

        # Send shipments from retailer to the final customer
        Final_Customer_Orders_Filled = Outbound_shipments[0]

        # Update the On-Hand Inventory to account for outflows
        OH_Inventory = New_OH_Inventory - Outbound_shipments

        # Determine Backlog, if any
        Inventory_Shortage = Order_flows[:, 0] - New_OH_Inventory
        New_Backorder = np.maximum(0, Backorder + Inventory_Shortage)
        Backorder = np.copy(New_Backorder)

        # Remember observed order but then Overwrite processed order flow to NaN for debuging if needed

        Observed_Order = np.copy(Order_flows[:, 0])
        # Order_flows[:, 0] = np.nan  ## ORIGINAL LINE OF CODE!!!! REPLACED TO AVOID NAN ISSUES !!!!!
        # Order_flows[:, 0] = 0

        #####Advance Order Slips and Brewers Brew######

        # Advance order slips
        Order_flows[:, 0] = Order_flows[:, (Information_Delay - 1)]
        # Order_flows[:, (Information_Delay - 1)] = np.nan

        # Brewers Brew
        Shipping_flows[3, (Shipping_Delay - 1)] = Production_Request

        #####PLACE ORDERS######

        for i in range(0, 4):

            Entity_Index = i

            # Obsrve the total supply line and the previous demand
            SL = sum(Shipping_flows[Entity_Index, :])
            L = Observed_Order[Entity_Index]

            # L hat is smoothing of observed demand from previous 2 periods
            # if t == 0:
            #    L_hat[Entity_Index] = np.copy(Observed_Order[Entity_Index])

            # Update L_hat (expected future orders) based on observed order
            L_hat_new = theta[Entity_Index] * L + (1 - theta[Entity_Index]) * L_hat[Entity_Index]
            L_hat[Entity_Index] = L_hat_new

            # Note stock of current inventory
            S = OH_Inventory[Entity_Index]

            #! Note stock of current inventory inclusive of backorder position
            S = OH_Inventory[Entity_Index] - Backorder[Entity_Index]

            # Add noise to the order if needed
            if (Noisey_Ordering == True):
                eps = np.random.normal(Noise_Mean, Noise_SD)
            else:
                eps = 0

            # AI Decision
            if (AI_Entity == True) and (Entity_Index == AI_Entity_Index):
                if (AI_Order != False):

                    #Check if agent decision is absolute or relative to the last order received
                    if (Relative_Ordering != True):
                        # here, agent action is absolute
                        Order_Placed = max(0,AI_Order)
                    else:
                        # here, the agent action is relative to the order received
                        Order_Placed = max(0,Order_Received[AI_Entity_Index] + AI_Relative_Order)

                else:
                    Order_Placed = max(0, L_hat[Entity_Index] + alpha_s[Entity_Index] * (
                            S_prime[Entity_Index] - S - beta[Entity_Index] * SL) + eps)


            else:
                Order_Placed = max(0, L_hat[Entity_Index] + alpha_s[Entity_Index] * (
                        S_prime[Entity_Index] - S - beta[Entity_Index] * SL) + eps)

            ##TURN ON FOR INTEGER ONLY ORDERING
            if Integer_Ordering:
                Order_Placed = np.round(Order_Placed, 0)

            if Entity_Index == 3:
                Production_Request = Order_Placed
            else:
                Order_flows[Entity_Index + 1, (Information_Delay - 1)] = Order_Placed

        # End of loop

        # Make orders placed by each entity explict
        Final_Orders[0:3] = Order_flows[1:, (Information_Delay - 1)]
        Final_Orders[3] = Production_Request

        fnt_output = {"Order_flows": Order_flows, "Shipping_flows": Shipping_flows, "OH_Inventory": OH_Inventory,
                      "Backorder": Backorder, "L_hat": L_hat, "Production_Request": Production_Request,
                      "Entity_Orders": Final_Orders, "Order_Received": Order_Received}

        return fnt_output

    # Resets the state space to the initial conditions for anothe repisode run
    # Note that the output format for this function MUST match the output for the step function
    # Any additional clean up or resetting of helper variables should occur eslewhere

    def reset(self):

        ##################
        # Assign and reset random game parameters
        ##################

        #### Randomly Draw new teammates if applicable
        if self.Random_Teams:
            # Randomly draw a team number
            Rand_Team = random.randint(min(self.Team_Index), max(self.Team_Index))

            # Create a mask of the rows that correspond to that team number
            Rand_Team_Mask = np.asarray(self.Team_Index) == Rand_Team

            # Filter, using the mask, the arrays that have the data for the team that was drawn
            Rand_Team_Theta = np.asarray(self.thetas)[Rand_Team_Mask]
            Rand_Team_Alpha = np.asarray(self.alphas)[Rand_Team_Mask]
            Rand_Team_Beta = np.asarray(self.betas)[Rand_Team_Mask]
            Rand_Team_S_prime = np.asarray(self.S_primes)[Rand_Team_Mask]

            # Assemble the team parameters into a named list for later use in the main Beer Game function
            Rand_Team_Parameter_df = {"theta": np.ndarray.tolist(Rand_Team_Theta),
                                      "alpha_s": np.ndarray.tolist(Rand_Team_Alpha),
                                      "beta": np.ndarray.tolist(Rand_Team_Beta),
                                      "S_prime": np.ndarray.tolist(Rand_Team_S_prime)}

            self.Parameter_df = Rand_Team_Parameter_df

        ## Assemble totally artifical bootstrapped teams if applicable
        if self.Bootstrapped_Teams:
            # Declare percision of results and the number of draws
            Rounding = 3
            Draws = 4

            # Define Ranges of each parameter
            Boot_Theta_Range = [0, 1]
            Boot_Alpha_Range = [0, 1]
            Boot_Beta_Range = [0, 1]
            Boot_S_prime_Range = [0, 40]

            Boot_Team_Theta = np.around(np.random.uniform(min(Boot_Theta_Range), max(Boot_Theta_Range), Draws),
                                        Rounding)
            Boot_Team_Alpha = np.around(np.random.uniform(min(Boot_Alpha_Range), max(Boot_Alpha_Range), Draws),
                                        Rounding)
            Boot_Team_Beta = np.round(np.random.uniform(min(Boot_Beta_Range), max(Boot_Beta_Range), Draws), Rounding)
            Boot_Team_S_prime = np.random.randint(min(Boot_S_prime_Range), max(Boot_S_prime_Range), Draws)

            # Assemble the team parameters into a named list for later use in the main Beer Game function
            Boot_Team_Parameter_df = {"theta": np.ndarray.tolist(Boot_Team_Theta),
                                      "alpha_s": np.ndarray.tolist(Boot_Team_Alpha),
                                      "beta": np.ndarray.tolist(Boot_Team_Beta),
                                      "S_prime": np.ndarray.tolist(Boot_Team_S_prime)}

            self.Parameter_df = Boot_Team_Parameter_df

        #### Randomly set game horizon if applicable

        if self.Random_Horizon == True:
            self.horizon = random.randint(self.min_horizon, self.max_horizon)
        else:
            self.horizon = self.fixed_horizon

        #### Randomly set the agent's position on the team
        if self.Random_AI_Position:
            self.AI_Entity_Index = random.randint(0, 3)
        else:
            self.AI_Entity_Index = self.AI_Position

        ##################
        # Resetting the global game parameters
        ##################

        # Reset the time period to t=0 for the beginning of the game
        self.period = 0

        # Reset the various stocks of material both wihtin and without each player's position
        self.Order_flows = np.full([4, 2], self.Initial_OrderFlows, dtype=float)
        self.Shipping_flows = np.full([4, 2], self.Initial_OrderFlows, dtype=float)
        self.OH_Inventory = [self.Initial_Inventory] * 4
        self.Backorder = [0] * 4
        self.Order_Received = [self.Initial_OrderFlows] * 4
        self.L_hat = [self.Initial_OrderFlows] * 4
        self.Order_History = np.full([4, self.horizon], 0, dtype=float)
        self.Service_rate = [0] * self.horizon
        self.OH_Inventory_History = np.full([4, self.horizon], 0, dtype=float)
        self.Backlog_History = np.full([4, self.horizon], 0, dtype=float)
        self.Production_Request = self.Initial_OrderFlows
        self.Final_Orders = [0] * 4  # delete?

        self.Amp_Vector = [0] * self.horizon  # delete?
        self.Reward_Vector = [0] * self.horizon  # delete?

        # Largely for later debugging and for record keeping, assemble the various items to reset at a global level
        #  together into a single list
        # Output = {"AI_Entity_Index": AI_Entity_Index, "Parameter_df": Parameter_df,"horizon": horizon, "period": period,
        #           "Orders": Orders, "Order_flows": Order_flows, "Shipping_flows": Shipping_flows,
        #           "OH_Inventory": OH_Inventory, "Backorder": Backorder, "L_hat": L_hat,
        #           "Order_History": Order_History, "Service_rate": Service_rate,
        #           "OH_Inventory_History": OH_Inventory_History, "Backlog_History": Backlog_History,
        #           "Production_Request": Production_Request, "Amp_Vector": Amp_Vector, "Reward_Vector": Reward_Vector}

        # Global_State = Output
        # globals().update(Global_State)

        ##################
        # Subset the global parameters to just those the agent is able to observe
        ##################

        # Subset the full global state to just the part the agent has access to

        Agent_Order_Received = self.Order_Received[self.AI_Entity_Index]
        Agent_OH_Inventory = self.OH_Inventory[self.AI_Entity_Index]
        Agent_Backorder = self.Backorder[self.AI_Entity_Index]

        if self.AI_Entity_Index == 3:
            Agent_Recent_Order = self.Production_Request
        else:
            Agent_Recent_Order = self.Order_flows[self.AI_Entity_Index + 1, (self.Information_Delay - 1)]

        AI_Entity_Index = self.AI_Entity_Index
        period = self.period

        #Construct the observation of the estimate of the other player's ordering parameters
        #First extract vector of values from Parameter Dataframe
        full_thetas = self.Parameter_df['theta']
        full_alphas = self.Parameter_df['alpha_s']
        full_betas = self.Parameter_df['beta']
        full_S_primes = self.Parameter_df['S_prime']

        #Remove the values from the base dataframe corresponding to the Agent's position
        trimmed_thetas = np.delete(full_thetas, AI_Entity_Index)
        trimmed_alphas = np.delete(full_alphas, AI_Entity_Index)
        trimmed_betas = np.delete(full_betas, AI_Entity_Index)
        trimmed_S_primes = np.delete(full_S_primes, AI_Entity_Index)

        OtherPlayer1_theta = trimmed_thetas[0]
        OtherPlayer2_theta = trimmed_thetas[1]
        OtherPlayer3_theta = trimmed_thetas[2]
        OtherPlayer1_alphas_s = trimmed_alphas[0]
        OtherPlayer2_alphas_s = trimmed_alphas[1]
        OtherPlayer3_alphas_s = trimmed_alphas[2]
        OtherPlayer1_beta = trimmed_betas[0]
        OtherPlayer2_beta = trimmed_betas[1]
        OtherPlayer3_beta = trimmed_betas[2]
        OtherPlayer1_S_prime = trimmed_S_primes[0]
        OtherPlayer2_S_prime = trimmed_S_primes[1]
        OtherPlayer3_S_prime = trimmed_S_primes[2]

        # Note: The observed state outputted by the reset function MUST match the shape as that from the step function
        #  and must ONLY consist of the parts of the global state the agent can actually observe
        Observed_State = np.array([Agent_Order_Received, Agent_OH_Inventory, Agent_Backorder,
                                   Agent_Recent_Order, period, AI_Entity_Index,
                                   OtherPlayer1_theta,
                                   OtherPlayer2_theta,
                                   OtherPlayer3_theta,
                                   OtherPlayer1_alphas_s,
                                   OtherPlayer2_alphas_s,
                                   OtherPlayer3_alphas_s,
                                   OtherPlayer1_beta,
                                   OtherPlayer2_beta,
                                   OtherPlayer3_beta,
                                   OtherPlayer1_S_prime,
                                   OtherPlayer2_S_prime,
                                   OtherPlayer3_S_prime
                                   ])
        return (Observed_State)
    # Takes the action by the agent, along with some simulation specific parameters, and updates the state
    # Note that the output format fo this function MUST match the output for the reset function
    # def step(self, action, Integer_Ordering, Noisey_Ordering, Parameter_df, AI_Entity_Index, CustOrders, horizon, period, OH_Inventory_History, Backlog_History):

    def step(self, action):

        # import globally assigned environmental variables
        global period, AI_Entity_Index

        # Check if the current period is the final one (the Horizon) and return dn=True for 'done' state
        # Recall that Python indexes starting at 0! So if Horizon is t=52, need to stop at period = 51
        if self.period == (self.horizon - 1):
            dn = True
        else:
            dn = False

        # Run the beer game function for a single step
        BeerGame_output = self.PirateBeerGame_funct(AI_Entity_Index=self.AI_Entity_Index, AI_Order=action,
                                                    Orders=self.Orders[self.period], Order_flows=self.Order_flows,
                                                    Shipping_flows=self.Shipping_flows, OH_Inventory=self.OH_Inventory,
                                                    Backorder=self.Backorder, L_hat=self.L_hat,
                                                    Production_Request=self.Production_Request,
                                                    AI_Entity=self.AI_Entity,
                                                    Noisey_Ordering=self.Noisey_Ordering,
                                                    Integer_Ordering=self.Integer_Ordering,
                                                    Parameter_df=self.Parameter_df)

        # Note on output of obove function call:
        # fnt_output = {"Order_flows": Order_flows, "Shipping_flows": Shipping_flows, "OH_Inventory": OH_Inventory,
        #              "Backorder": Backorder, "L_hat": L_hat, "Production_Request": Production_Request,
        #              "Entity_Orders": Final_Orders, "Order_Received": Order_Received}

        self.Order_flows = BeerGame_output['Order_flows']
        self.Shipping_flows = BeerGame_output['Shipping_flows']
        self.OH_Inventory = BeerGame_output['OH_Inventory']
        self.Backorder = BeerGame_output['Backorder']
        self.L_hat = BeerGame_output['L_hat']
        self.Production_Request = BeerGame_output['Production_Request']
        self.Order_Received = BeerGame_output['Order_Received']

        # Don't use 'Entity_Orders' output right now

        info = dict()

        # Reward in any time other than the final time is the cost incurred by the AI that round.
        # But in the final state, it's the total cost incurred by the entire team!

        # Calculation of the running cost incurred so far for the entire team...
        self.OH_Inventory_History[:, self.period] = BeerGame_output['OH_Inventory']
        self.Backlog_History[:, self.period] = BeerGame_output['Backorder']

        # Calculation of the cost incurred by the AI for just this one period...
        Period_OH_Inventory = BeerGame_output['OH_Inventory']
        Period_Backorder = BeerGame_output['Backorder']

        AI_period_OH_Inventory = Period_OH_Inventory[self.AI_Entity_Index]
        AI_period_Backorder = Period_Backorder[self.AI_Entity_Index]
        AI_period_cost = AI_period_OH_Inventory * self.Holding_Cost + AI_period_Backorder * self.Backorder_Cost
        AI_Reward = -AI_period_cost

        reward = AI_Reward

        #In final round, reward is total team cost, offset by costs incurred by AI so far in order to
        # to make the entire episode cost the standard team cost
        if dn == True:

            Costs_Per_Period = self.OH_Inventory_History * self.Holding_Cost + self.Backlog_History * self.Backorder_Cost
            Total_Costs_Per_Entity = np.sum(Costs_Per_Period, 1)
            Total_Team_Costs = sum(Total_Costs_Per_Entity)
            Team_Reward = -Total_Team_Costs

            reward = Team_Reward #+ Total_Costs_Per_Entity[self.AI_Entity_Index]

            #normalize final reward by the horizon
            #if self.Random_Horizon == True:
            #    reward = reward/self.horizon

            reward = reward / self.horizon

        # Alt reward calculation
        #reward = AI_Reward + Team_Reward / (self.period + 1)  # Team_Reward matters more and more as time goes on?

        #### Subset the global state to just the parts the agent has access to

        Agent_Order_Received = self.Order_Received[self.AI_Entity_Index]
        Agent_OH_Inventory = self.OH_Inventory[self.AI_Entity_Index]
        Agent_Backorder = self.Backorder[self.AI_Entity_Index]

        if self.AI_Entity_Index == 3:
            Agent_Recent_Order = self.Production_Request
        else:
            Agent_Recent_Order = self.Order_flows[self.AI_Entity_Index + 1, (self.Information_Delay - 1)]

        AI_Entity_Index = self.AI_Entity_Index

        # Add to the period number
        self.period += 1
        period = self.period

        #Construct the observation of the estimate of the other player's ordering parameters
        #First extract vector of values from Parameter Dataframe
        full_thetas = self.Parameter_df['theta']
        full_alphas = self.Parameter_df['alpha_s']
        full_betas = self.Parameter_df['beta']
        full_S_primes = self.Parameter_df['S_prime']

        #Remove the values from the base dataframe corresponding to the Agent's position
        trimmed_thetas = np.delete(full_thetas, AI_Entity_Index)
        trimmed_alphas = np.delete(full_alphas, AI_Entity_Index)
        trimmed_betas = np.delete(full_betas, AI_Entity_Index)
        trimmed_S_primes = np.delete(full_S_primes, AI_Entity_Index)

        OtherPlayer1_theta = trimmed_thetas[0]
        OtherPlayer2_theta = trimmed_thetas[1]
        OtherPlayer3_theta = trimmed_thetas[2]
        OtherPlayer1_alphas_s = trimmed_alphas[0]
        OtherPlayer2_alphas_s = trimmed_alphas[1]
        OtherPlayer3_alphas_s = trimmed_alphas[2]
        OtherPlayer1_beta = trimmed_betas[0]
        OtherPlayer2_beta = trimmed_betas[1]
        OtherPlayer3_beta = trimmed_betas[2]
        OtherPlayer1_S_prime = trimmed_S_primes[0]
        OtherPlayer2_S_prime = trimmed_S_primes[1]
        OtherPlayer3_S_prime = trimmed_S_primes[2]

        # Note: The observed state outputted by the reset function MUST match the shape as that from the step function
        #  and must ONLY consist of the parts of the global state the agent can actually observe
        Observed_State = np.array([Agent_Order_Received, Agent_OH_Inventory, Agent_Backorder,
                                   Agent_Recent_Order, period, AI_Entity_Index,
                                   OtherPlayer1_theta,
                                   OtherPlayer2_theta,
                                   OtherPlayer3_theta,
                                   OtherPlayer1_alphas_s,
                                   OtherPlayer2_alphas_s,
                                   OtherPlayer3_alphas_s,
                                   OtherPlayer1_beta,
                                   OtherPlayer2_beta,
                                   OtherPlayer3_beta,
                                   OtherPlayer1_S_prime,
                                   OtherPlayer2_S_prime,
                                   OtherPlayer3_S_prime
                                   ])

        return Observed_State, reward, dn, info

## Main Code

if __name__ == '__main__':

    from gym import Env, spaces

    # Import methods to build DQN agent
    from keras.models import Sequential, Model
    from keras.layers import Dense, Activation, Flatten, Input, Concatenate
    from keras.optimizers import Adam

    from rl.agents.dqn import DQNAgent
    from rl.policy import BoltzmannQPolicy,MaxBoltzmannQPolicy, EpsGreedyQPolicy
    from rl.memory import SequentialMemory

    #Set the working directory to the current script location
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    # Get environment and set seed for reproduceability
    env = BeerGameEnv()

    Set_Random_Seed = True

    if Set_Random_Seed:
        Random_Seed = 11111111
        #Random_Seed = 19861026
        os.environ['PYTHONHASHSEED'] = '0'
        np.random.seed(Random_Seed)
        random.seed(Random_Seed)
        tf.random.set_seed(Random_Seed)
        env.action_space.seed(Random_Seed)


    # Count number of actions
    nb_actions = env.action_space.n

    # Build build simple model.

    WINDOW_LENGTH = 4

    input_shape = env.observation_space.shape

    model = Sequential()
    model.add(Flatten(input_shape = (WINDOW_LENGTH,) + env.observation_space.shape))
    model.add(Dense(256))
    model.add(Activation('relu'))
    model.add(Dense(128))
    model.add(Activation('relu'))
    model.add(Dense(64))
    model.add(Activation('relu'))
    model.add(Dense(nb_actions))
    model.add(Activation('linear'))
    print(model.summary())

    # Configure and compile the DQN agent
    memory = SequentialMemory(limit=2000, window_length=WINDOW_LENGTH)
    policy = BoltzmannQPolicy()
    policy = MaxBoltzmannQPolicy()
    #policy = EpsGreedyQPolicy()
    #Note, Boltzman policy and DQN is overestimating Q values, causing probabilitiies to explode...
    #Double DQN helps mitigate this Q-value overestimation a bit
    #Dueling networks appear to allow for a full run
    dqn = DQNAgent(model=model, nb_actions=nb_actions, memory=memory, nb_steps_warmup=500,
                   target_model_update=1e-2, policy=policy, enable_dueling_network=True, dueling_type='avg')
    dqn.compile(Adam(lr=1e-4), metrics=['mae'])

    mode = "Train"
    #mode = "Test"

    if mode == "Train":
        now = datetime.datetime.now()
        dt_string = now.strftime("%Y%m%d_%H%M%S")
        ENV_NAME = "Beer_Game_Stocastic_DQN"

        print('Training model....')
        Full_Hist = dqn.fit(env, nb_steps=2e6, visualize=False, verbose=2)
        Training_History = Full_Hist.history

        #wieght_filename = f'dqn_{ENV_NAME}_{dt_string}_weights.h5f'
        wieght_filename = 'dqn_test_fit.weights'
        model_filename='dqn_test_fit_wide'
        model_filename = 'dqn_test_fit'
        model_filename = 'MB_Entity0_SmallWindow'

        dqn.save_weights(wieght_filename, overwrite=True)
        dqn.model.save(model_filename, overwrite=True)
        print('Training completed! Testing and printing Average Episodic Rewards')
        dqn.test(env, nb_episodes=10, visualize=False)

        avg_reward_list = Training_History['episode_reward']
        plt.plot(avg_reward_list)
        plt.xlabel("Episode")
        plt.title("Avg. Episodic Reward")
        plt.show()

    if mode == "Test":

        ### Load a saved and trained model

        #wieght_filename = "ddpg_Beer_Game_Stocastic_DQN_20210614_115704_weights.h5f"
        wieght_filename = 'dqn_test_fit.weights'
        model_filename = 'dqn_test_fit.model'

        #Trained_DQN = tf.saved_model.load(model_filename)
        #dqn.load_weights(wieght_filename)
        #test_out = dqn.test(env,nb_episodes=10, visualize=False)

        agent = tf.keras.models.load_model(model_filename)

        #Get the implied window size used when originally training the loaded model:
        model_input_shape = (agent.get_layer(index=0).output_shape)[0]  #Get the shape attribute from the input layer
        Original_Batch_Size = model_input_shape[0]                      #First number is the number of items looked as simulateously
        Original_Window_Size = model_input_shape[1]                     #Second number is the window used for any sequential memory
        Original_Observation_Size = model_input_shape[2]                #Third number and (and onwards for multi dimensional inputs) is the actual observed space

        sub_mode = "Full"
        sub_mode = "Single"

        if sub_mode == "Single":
            ###Test for single observation:

            #Observed_State = np.array([Agent_Order_Received, Agent_OH_Inventory, Agent_Backorder,
            #                          Agent_Recent_Order, period, AI_Entity_Index])

            #Note: need to muliply by window length!
            obs = np.array([4, 0, 5,
                            4, 10, 2])

            #Extract the order received from the observations set for use in relative ordering
            Agent_Order_Received = obs[1]

            #Expand initial observation out to fill history or window length
            obs = np.tile(obs,(Original_Window_Size,1))

            #Coerce the 1-D observation input into a 3-D array that TensorFlow will flattend and accept
            resized_obs = obs[np.newaxis, ...]

            qmatrix = agent.predict(resized_obs)
            flattened_q = np.ndarray.flatten(qmatrix)
            BestChoice = np.argmax(flattened_q)

            Relative_Order = BestChoice + env.Relative_Min_Order # + 1 double check this plus one here...
            Agent_Order = max(0, Agent_Order_Received + Relative_Order)

            print("Agent Order:")
            print(Agent_Order)

        if sub_mode == "Full":

            horizon = 120

            reset_list = reset(horizon=horizon)
            locals().update(reset_list)

            Parameter_df = {"theta": [0.36] * 4,
                            "alpha_s": [0.26] * 4,
                            "beta": [0.34] * 4,
                            "S_prime": [17] * 4}

            Holding_Cost = 0.50
            Backorder_Cost = 1.00

            AI_Entity = False
            AI_Entity_Index = False
            AI_Order = False
            Integer_Ordering = True
            Noisey_Ordering = False

            for t in range(0, (horizon)):

                if t >= 20:
                    t = t

                # NESTED FUNCTION TO RUN THE GAME FOR ONE TIME STEP AND RETURN THE NEW STATE
                BeerGame_output = PirateBeerGame_funct(AI_Entity_Index=AI_Entity_Index, AI_Order=AI_Order,
                                                       Orders=Orders[t],
                                                       Order_flows=Order_flows,
                                                       Shipping_flows=Shipping_flows, OH_Inventory=OH_Inventory,
                                                       Backorder=Backorder, L_hat=L_hat,
                                                       Production_Request=Production_Request, AI_Entity=AI_Entity,
                                                       Noisey_Ordering=Noisey_Ordering,
                                                       Integer_Ordering=Integer_Ordering,
                                                       Parameter_df=Parameter_df)

                locals().update(BeerGame_output)

                # Write values for analysis/plotting later
                Order_History[:, t] = Entity_Orders
                OH_Inventory_History[:, t] = OH_Inventory
                Backlog_History[:, t] = Backorder
                Service_rate[t] = Final_Customer_Orders_Filled / Orders[t]

            # Calculate costs
            Net_Inventory = OH_Inventory_History - Backlog_History
            Costs_Per_Period = OH_Inventory_History * Holding_Cost + Backlog_History * Backorder_Cost
            Total_Costs_Per_Entity = np.sum(Costs_Per_Period, 1)
            Total_Team_Costs = sum(Total_Costs_Per_Entity)

            print(Order_History)
            print(Total_Team_Costs)

            ###GRAPHS###
            import matplotlib.pyplot as plt

            x = range(0, horizon)

            plt.figure(1)
            PlotObj = plt.plot(Order_History.T)
            plt.title('Orders per Period')
            plt.xlabel('Time')
            plt.ylabel('Orders')
            # showing legend
            plt.legend(iter(PlotObj), ('0: Retailer', '1: Wholesaler', '2: Distributor', '3: Factory'))

            plt.figure(2)
            PlotObj = plt.plot(Net_Inventory.T)
            plt.title('Net Inventory per Period')
            plt.xlabel('Time')
            plt.ylabel('Net Inventory (On-Hand less Backlog)')
            # showing legend
            plt.legend(iter(PlotObj), ('0: Retailer', '1: Wholesaler', '2: Distributor', '3: Factory'))

            plt.show()
